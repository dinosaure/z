# Decompress (Part One)

One of the most used standard is [RFC1951][rfc1951] into several ways. In fact,
when you launch your Linux kernel, it inflates itself according [zlib][zlib]
standard, a superset of [RFC1951][rfc1951].

It used by [Git][git] too to pack your objects into a PACK file. As a request of
@samoht, `decompress` appeared some years ago as an implementation of
[zlib][zlib] in OCaml and as a replacement to be able to compile a MirageOS
_unikernel_ with [ocaml-git][ocaml-git]. And today, this little project passes the
major release with plenty of works in several domains.

`decompress` is a complex library where we need to take care about several
details like memory assumptions or performances. Deal with both can be hard and
OCaml comes with some difficulties - notably, the garbage collection.

However, OCaml can protect the user against lot of bugs which can be highlight
by the type-system __and__ by the runtime too - like bounds checking.
`ocaml-tls`, as a response of the famous [failure][openssl-fail] of `openssl`,
was done in part in this view.

`decompress` is one project of the MirageOS _unikernel_ to be able to inflate
and deflate a _flow_. The goal is to provide an agnostic library (which should
be compile on any platforms - including JavaScript). We surely can not do better
than some others works like [zstd][zstd] or [lz4][lz4] but the main goal is to
provide an other implementation as a comparison point with the existing.

So, in this first article, we will explain deeply the design of the new version
of `decompress`.

## API

The API should be the most difficult work about a library - it reveals what we
should can do and how we should do. By this way, an API should:

- constraint the user to avoid security issues
- imply an optimal design of how to use it
- allow some degrees of freedom to fit under hostile environment

Of course, the question is hard when too much freedom can come with a misuse of
the API - an example is [dune][dune] which wants consciously to limit the user
about what he can do with it.

A misuse of the API can come with security issues, `Hashtbl` is an example where
a `random:false` uses a fixed hash function. By this way, collisions between
keys happen deterministically - and can be exploited by an attacker.

[digestif][digestif] is another example which provided an `unsafe_compare`
instead the common `compare` function (before `eqaf.0.5`). By this way, it
enforces the user to do an alias of it if he wants to use an hash in a `Map` -
however, by this operation, he should know that he is not protected by a
timing-attack.

However, a constrained API can not fit under an hostile context. Let the user to
choose some details, like length of inputs buffer for a stream computation can
be relevant if you want to compile your _unikernel_ for an [ESP32][esp32]
target.

Possibilities should deserve the user and, even if they come with difficulties
(about comprehension - and this is why the documentation is important), fit for
some special contexts like a server - where memory consumption should be
deterministic.

### A dbuenzli's API

From our experience about protocol/format, one design deserves our applications:
the _dbuenzli's API_. If you look into some famous libraries inside the OCaml
eco-system, you probably know [uutf][uutf], [jsonm][jsonm] or [xmlm][xmlm].

All of these libraries provide the same design about how to compute an
Unicode/JSON/XML flow - of course, details are not the same.

From a MirageOS perspective, even if they use the `in_channel`/`out_channel`
abstraction (which differs from the [flow][mirage-flow] idea), these libraries
are agnostic to the system when they let the user to choose inputs and outputs
buffer (with the `Manual` case). Most important,they don't use the `Unix` module
which is available on the OCaml distribution but can not be used into an
_unikernel_.

Then, the API are pretty-consistent and try to do the _best-effort_ about
decoding. The side-effect characteristic of _states_ implies an interesting
design which can be opportunistic on some optimizations from the compiler.
Internally, of course, we have a _porcelain_ implementation where any details
can have an rational explanation.

At the beginning, `decompress` wanted to follow the same interface without the
mutability (a choice about performances) and it did. Then, the hard test was to
use it in another bigger project. In this case, [ocaml-git][ocaml-git] and a
retro-active loop started to get what we really needed, what we should not
provide (like special cases) and what we should provide to reach an uniform API
where it's not difficult to determine his behaviors.

From this experience, we approved the `decompress` API and it did not change in
general way for 4 versions (2 years).

### The new `decompress` API

`decompress` wants to keep the same logic than before about the inflator, but it
changes drastically the deflator where the _flush_ operation was not really
clear. For many purposes, it's enough where people don't want to craft by
themseves their compressed flows systematically - they mostly want an
`of_string`/`to_string` function.

However, in some contexts, like a PNG encoder/decoder, user should be able to
play with `decompress` in details (OpenPGP wants it too in [rfc4880][rfc4880]).

#### Zlib and RFC1951

The _zlib_ format, a superset of the RFC1951 format is easy to understand. We
will talk about the RFC1951 only where [zlib][zlib] adds only few informations
(like a checksum of the flow).

The format consists to several blocks, DEFLATE blocks where they have a little
header and then, the contents. So we can have 3 kinds of DEFLATE blocks:
- a FLAT block (no compression, just a _blit_ from inputs to the current block)
- a FIXED block (compression from a already computed _huffman_ coding)
- a DYNAMIC block (compression from a specified _huffman_ coding)

To understand correctly the idea behind `decompress` or [zlib][zlib], we need to
do a short introduction about [_huffman_ coding][huffman-coding]. It consists to
make a dictionary between a byte (like `'6'`) and a suit of a variable-length
bits. The common idea is to produce from the most common byte, the smallest bit
sequence - and vice-versa, the less common byte binded to the largest bit
sequence.

From this dictionary, we just translate byte per bits sequence and we should
have a good ratio of compression. Of course, we have some other _details_ like
an _huffman_ coding is prefix-free. Then, the compression go furthermore with
the [lz77][lz77] algorithm.

But at the end, for a DYNAMIC block, we need to transmit this dictionary which
will be compressed too inside the current BLOCK. Inflator will decompress it and
be able to do the _reverse_ translation between bits sequence and byte.

Finally, on the header, we have a bit which inform the inflator if it's the last
block (and stop the process then) or not.

#### Inflator

The design of the inflator did not change a lot from the last version of
`decompress`. Indeed, it's about to take an input, compute it and return an
output like a flow. Of course, the error case can be reached.

So the API is pretty-easy:

```ocaml
val decode : decoder -> [ `Await | `Flush | `End | `Malformed of string ]
```

As you can see, we have 4 cases, one which expects more inputs (`Await`), the
second which ask to the user to flush internal buffer (`Flush`), the `End` case
when we reach the end of the flow and the `Malformed` case when we reach an
error.

For each case, the user can do several operations. Of course, about the `Await`
case, he can refill the contents with an other inputs buffer with:

```ocaml
val src : decoder -> bigstring -> off:int -> len:int -> unit
```

This function provides to the decoder a new input with `len` bytes to read
starting at `off` in the given `bigstring`.

On the `Flush` case, the user wants some informations like how many bytes are
available in the current outputs buffer. Then, we should provide an action to
_flush_ this outputs buffer. At the end, this outputs buffer should be given by
the user (how many bytes he wants to allocate to store outputs flow).

```ocaml
type src = [ `Channel of in_channel | `Manual | `String of string ]

val dst_rem : decoder -> int
val flush : decoder -> unit
val decoder : src -> o:bigstring -> w:bigstring -> decoder
```

The last function, `decoder`, is the most interesting. It let the user, at the
beginning, to choose in which context he wants to inflate inputs. So he chooses:
- `src`, where come from inputs flow
- `o`, outputs buffer
- `w`, window buffer

`o` will be used to store inflated outputs, `dst_rem` will give to us how many
bytes inflator stored in `o` and `flush` will just set `decoder` to be able
to recompute the flow.

`w` is needed about the [lz77][lz77] compression. However, as we said, we let
the user to give to us this intermediate buffer. The idea behind that is to let
the user to prepare an _inflation_. For example, in [ocaml-git][ocaml-git],
instead to allocate `w` systematically when we want to decompress a Git object,
we allocate `w` one time per threads and all are able to use it and __re-use__
it. By this way, we avoid a systemaical allocation (and allocate only once time)
which can have a serious impact about performances.

The design is pretty-close to one idea, a _description_ step by the `decoder`
function and a real computation loop with the `decode` function. The idea is to
prepare the inflation with some informations (like `w` and `o`) before the main
(and the most expensive) computation. Internally we do that too (but it's mostly
about a macro-optimization).

It's the purpose of OCaml in general, be able to have a powerful way to describe
something (with constraints). In our case, we are very limited to what we need
to describe. But, in others libraries like [angstrom][angstrom], the description
step is huge (describe the parser according to the BNF) and then, we use it to
the main computation, in the case of [angstrom][angstrom], the parsing (another
example is [cmdliner][cmdliner]).

This is why `decoder` can be considered as the main function where `decode` can
be wrapped under a stream.

#### Deflator

The deflator is a new (complex) deal. Indeed, behind it we have two concepts:
- the encoder (according to RFC1951)
- the compressor

For this new version of `decompress`, we decide to separate these concepts where
one question leads all: how to put my compression algorithm? (instead to use
[lz77][lz77]).

In fact, if you are interesting about compression, several algorithms exist and,
in some context, it's preferable to use [lzwa][lzwa] for example or rabin's
fingerprint (with [duff][duff]), etc.

##### Functor

The first idea was to make a _functor_ which expects an implementation of the
compression algorithm. However, a _functor_ comes with a (big?) regression about
performance (mostly an indirection):

```ocaml
module type S = sig type t val add : t -> t -> t val one : t end
module Make (S : S) = struct let succ x = S.add x S.one end
module Int = struct type t = int let add a b = a + b let one = 1 end

include Make(Int)

let f x = succ x
```

Currently, with OCaml 4.07.1, the `f` function will be a `caml_apply2` where we
can expect a simple _inlining_ and become an `addq $2, %rax` (note that
`flambda` does this optimization):

```cmm
(function{main.ml:7,6-16} camlMain__f_1018 (x/1019: val)
 (let env/1043 (load_mut val (+a "camlMain" 16))
   (app{main.ml:2,42-55} "caml_apply2" x/1019
     (load_mut val (+a (load_mut val (+a env/1043 16)) 8))
     (load_mut val (load_mut val (+a env/1043 16))) val)))
```

Optimization on _functor_ is hard and if we think about performance, we should
get an other strategy to let the user to use his compression algorithm. From
Pierre CHambart's advise, _functor_ can be optimized currently on OCaml but
needs to respect several constraints: and it seems hard to fit under these
constraints and let the compiler to optimize it.

##### Split encoder and compressor

So, the choice was done to split the encoder which respects RFC1951 and the
compressor under some constraints. However, this is not what [zlib][zlib] did
and, by this way, we decided to provide a new design/API which did not follow,
in first instance, [zlib][zlib] (or some others implementations like
[miniz][miniz]).

To be fair, the choice from [zlib][zlib] and [miniz][miniz] comes from the first
point about API and the context where they are used. The main problem is the
shared queue between the encoder and the compressor. In a C code, it can be hard
for the user to deal with it (where he is liable by a buffer overflow).

In OCaml and for `decompress`, the shared queue can be well-abstracted and API
can ensure assumptions (like bounds checking).

Even if this design is much more complex than before, coverage tests are better
where we can separately test the encoder and the compressor. It breaks down the
initial black-box where compression was intrinsec with encoding - which was
error-prone. Indeed, `decompress` had a bug about generation of
[huffman-coding][huffman-coding] but we never reached it because the (bad)
compressor was not able to produce something (a specific lengh with a specific
distance) to get it.

NOTE: You have just read the main reason about the new version of `decompress`!

##### The compressor

The compressor is the most easy part. The goal is to produce from an inputs
flow, an outputs flow which is an other (more compacted) representation. This
representation consists to:
- A _literal_, the byte as is
- A _copy_ code with an _offset_ and a _length_

The last one say to copy _length_ byte(s) from _offset_. For example, `aaaa` can
be compressed as `[ Literal 'a'; Copy (offset:1, len:3) ]`. By this way, instead
to have 4 bytes, we have only 2 elements which will be compressed then by an
[_huffman_ coding][huffman-coding]. This is the main idea of the [lz77][lz77]
compression.

However, the compressor should need to deal with the encoder. An easy interface,
_à la [uutf][uutf]_ should be:

```ocaml
val compress : state -> [ `Literal of char | `Copy of (int * int) | `End | `Await ]
```

But as I said, we need to feed a queue instead.

----

At this point, purpose of the queue is not clear and not really explained. The
signature above still is a valid and understable design. Then, we can imagine to
pass `Literal` and `Copy` directly to the encoder. However, we should (for a
performance purpose) use a delaying tactic between the compressor and the
deflator[^1].

Behind this idea, it's to be able to implement an _hot-loop_ on the encoder
which will iter inside the shared queue and _transmit_/_encode_ contents
directly to the outputs buffer.

[1]: About that, you should be interesting by the reason of [why GNU yes is so fast][yes] where the secret is just about buffering.

----

So, when we make a new `state`, we let to the user to give his queue:

```
val state : src -> w:bistring -> q:queue -> state
val compress : state -> [ `Flush | `Await | `End ]
```

The `Flush` case appears when the queue is full. Then, we refind the `w` window
buffer which is needed to produce the `Copy` code. A _copy code_ is limited
according RFC1951 where _offset_ can not be upper than the length of the window
(commonly 32ko). _length_ is limited too to `258` (an arbitrary choice).

Of course, about the `Await` case, the compressor comes with a `src` function as
the inflator. Then, we added some accessors, `literals` and `distances`. The
compressor does not build the [_huffman_ coding][huffman-coding] which needs
frequencies, so we need firstly to keep counters about that inside the state and
a way to get them (and pass them to the encoder).

##### The encoder

Finally, we can talk about the encoder which will take the shared queue filled
by the compressor and provide an RFC1951 compliant outputs flow.

However, we need to talk about a special _detail_. When we want to make a
DYNAMIC block from frequencies and then encode the inputs flow, we can reach a
case where the shared queue contains an _opcode_ (a _literal_ or a _copy_) which
does not appear in our dictionary.

In fact, if we want to encode `[ Literal 'a'; Literal 'b' ]`, we will not try to
make a dictionary which will contains the 256 possibilities of a byte but we
will only make a dictionary from frequencies which contains only `'a'` and
`'b'`. By this way, we can reach a case where the queue contains an _opcode_
(like `Literal 'c'`) which can not be encoded by the _already decided_
[_huffman_ coding][huffman-coding] - remember, the DYNAMIC block __starts__ with
the dictionary.

An other point is about inputs. The encoder expects, of course, contents from
the shared queue but it wants from the user the way to encode contents: which
block we want to emit. So it has two entries:
- the shared queue
- an _user-entry_

So for many real tests, we decided to provide this kind of API:

```ocaml
type dst = [ `Channel of out_channel | `Buffer of Buffer.t | `Manual ]

val encoder : dst -> q:queue -> encoder
val encode : encoder -> [ `Block of block | `Flush | `Await ] -> [ `Ok | `Partial | `Block ]
val dst : encoder -> bigstring -> off:int -> len:int -> unit
```

As expected, we take the shared queue to make a new encoder. Then, we let the
user to specify which kind of block he wants to encode by the `Block`
operation.

The `Flush` operation is to try to encode all elements present inside the shared
queue according the current block and feed the outputs buffer. From it, the
encoder can returns some values:
- `Ok` and the encoder encoded all _opcode_ from the shared queue
- `Partial`, the outputs buffer is not enough to encode all _opcode_, the user
  should flush it and give to us a new empty buffer with `dst`. Then, he must
  continue with the `Await` operation.
- `Block`, the encoder reachs an _opcode_ which can not be encoded with the
  current block (the current dictionary). Then, he must continue with a new
  `Block` operation.
  
The hard part is about the _ping-pong_ game between the user and the encoder
where a `Block` expect a `Block` response from the user and a `Partial` expects
an `Await` response. But this design reveals something higher about [zlib][zlib]
this time: the _flush_ mode.

##### The _flush_ mode

Firstly, we talk about _mode_ because [zlib][zlib] does not allow the user to
decide what he wants to do when we reach a `Block` or a `Ok` case. So, it
defines some [under-specified _modes_][deflate-flush] to apply a politic of how
to do in this case.

In `decompress`, we followed the same design and see that it may be not a good
idea where the logic is not very clear and the user wants may be an another
behavior. It was like a _black-box_ with a _black-magic_.

Because we decided to split encoder and compressor, the idea of the _flush mode_
does not exists anymore where the user explicitly needs to give to the encoder
what he wants (make a new block? which block? keep frequencies?). So we broke
the _black-box_. But, as we said, it was possible mostly because we can abstract
safely the shared queue between the compressor and the encoder.

OCaml is an expressive language and we can really talk about a queue where, in
C, it will be just an other _array_. As we said, the deal is about performance,
but now, we let the user the possibilitie to write his code in this corner-case
which is when he reachs `Block`. Behaviors depends only on him.

### About API in general

Daniel Bünzli said that the biggest work when you make your library is about API.

An API should be a nice deal between what we can do and what is wrong. From our
experience, several times, some people said that our APIs are complex - and they
are certainly right. However, try, as a first shot, to provide an _easy_ API
force the user not to ask the right questions about security issues, memory
consumptions, performances.

An `of_string`/`to_string` can be done by the current API of `decompress`, but
the opposite is not true - you definitely can not have a stream API with a
`of_string`/`to_string`.

In other side, let the user to do everything is the wrong way, in any case, this
way is error-prone.

So the best advise about how to write a library is keep in your mind what you
__really__ want - and you will discover step by step the real issue, and, by
this way, the real reason of your work. The retro-active loop when you try to
use your library is the most important because only this can highlight bad
design, corner-cases and details.

Then, use and re-use it on your tests (important!) and inside higher projects
give you interesting questions about your design. In the last version of
`decompress`, mostly because _flush mode_ was not really clear, we never used it
into [ocaml-git][ocaml-git] - that mostly means the unusability of this API.

[rfc1951]: https://tools.ietf.org/html/rfc1951
[zlib]: https://zlib.net/
[git]: https://git-scm.com/
[ocaml-git]: https://github.com/mirage/ocaml-git/
[zstd]: https://github.com/facebook/zstd
[lz4]: https://github.com/lz4/lz4 
[dune]: https://github.com/ocaml/dune
[digestif]: https://github.com/mirage/digestif
[esp32]: https://mirage.io/blog/2018-esp32-booting
[uutf]: https://github.com/dbuenzli/uutf
[jsonm]: https://github.com/dbuenzli/jsonm
[xmlm]: https://github.com/dbuenzli/xmlm 
[mirage-flow]: https://github.com/mirage/mirage-flow
[rfc4880]: https://tools.ietf.org/html/rfc4880
[huffman-coding]: https://zlib.net/feldspar.html
[lz77]: https://en.wikipedia.org/wiki/LZ77_and_LZ78
[lzwa]: https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Markov_chain_algorithm 
[duff]: https://github.com/mirage/duff 
[miniz]: https://github.com/richgel999/miniz
[yes]: https://www.reddit.com/r/unix/comments/6gxduc/how_is_gnu_yes_so_fast/
[deflate-flush]: https://www.bolet.org/~pornin/deflate-flush.html
[gcc-strlen]: https://stackoverflow.com/a/55589634
[jst-gc-bit]: https://blog.janestreet.com/what-is-gained-and-lost-with-63-bit-integers/
[base64]: https://tarides.com/blog/2019-02-08-release-of-base64.html
[cstruct-ppx]: https://github.com/mirage/ocaml-cstruct#ppx
[bitstring]: https://bitbucket.org/thanatonauts/bitstring/src
[angstrom]: https://github.com/inhabitedtype/angstrom
