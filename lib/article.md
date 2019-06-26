# Decompress

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

So, in this article, we will see what is done and describe some details mostly
about the way to make a MirageOS-compatible library and how to optimize some
snippets. Let's talk about design, optimization and ready-to-use works!

## API

The API should be the most difficult work about a library - it reveals what we
should can do how we should do. By this way, an API should:

- constraint the user to avoid security issues
- imply an optimal design of how to use it
- allow some degrees of freedom to fit under hostile environment

Of course, the question is hard when too much freedom can come with a misuse of
the API - an example is [dune][dune] which wants consciously to limit the user
about what he can do with it.

A misuse of the API can come with security issues, `Hashtbl` is an example where
a `random:false` uses a fixed hash function. By this way, collisions between
keys happen deterministically - and can be exploited by an attacker.

[digestif][digestif] is another example which provides an `unsafe_compare`
instead the common `compare` function. By this way, it enforces the user to do
an alias if he wants to use an hash in a `Map` - however, by this operation, he
should know that he is not protected by a timing-attack.

However, a constrained API can not fit under an hostile context. Let the user to
choose some details, like length of inputs buffer for a stream computation can
be relevant if you want to compile your _unikernel_ for an [ESP32][esp32]
target.

Possibilities should desserve the user and, even if they come with difficulties
(about comprehension), fit for some special contexts like a server - where
memory consumption should be deterministic.

### A dbuenzli's API

From our experience about protocol/format, one design deserves our applications:
the _dbuenzli's API_. If you look into some famous libraries inside the OCaml
eco-system, you probably know [uutf][uutf], [jsonm][jsonm] or [xmlm][xmlm].

All of these libraries provide the same design about how to compute an
Unicode/JSON/XML flow - of course, details are not the same.

From a MirageOS perspective, even if they use the `in_channel`/`out_channel`
abstraction (which differs from the [flow][mirage-flow] idea), these libraries
are agnostic to the system when they let the user to choose inputs and outputs
buffer (with the `Manual` case).

Then, the API are pretty-consistent and try to do the _best-effort_ about
decoding. The side-effect characteristic of _states_ implies an interesting
design which can be opportunistic on some optimizations from the compiler.
Internally, of course, we have a _porcelain_ implementation where any details
can have an rational explanation.

At the beginning, `decompress` wanted to follow the same interface without the
mutability (a choice about performances) and it did. Then, the hard test was to
use it in another bigger project. In this case, [ocaml-git][ocaml-git] and a
retro-active loop started to get what we really needed, what we should not to
provide (like special cases) and provide to reach an uniform API where it should
not be difficult to determine his behaviors.

From this experience, we approved the `decompress` API and it did not change in
general way for 4 versions (2 years).

### The new `decompress` API

`decompress` wants to keep the same logic than before about the inflator, but it
changes drastically the deflator where the _flush_ operation was not really
clear. For many purposes, it's enough where people don't want to craft by
himself their compressed flows systematically - they mostly want an
`of_string`/`to_string` function.

However, in some contexts, like a PNG encoder/decoder, user should be able to
play with `decompress` in details (OpenPGP wants it too in [rfc4880][rfc4880]).

#### Zlib and RFC 1951

The _zlib_ format, a superset of the RFC 1951 format is easy to understand. We
will talk about the RFC 1951 only where [zlib][zlib] adds only few informations
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

From this dictionary, we just translate byte per bit sequence and we should have
a good ratio of compression. Of course, we have some other _details_ like an
_huffman_ coding is prefix-free. Then, the compression go furthermore with the
[lz77][lz77] algorithm.

But at the end, for a DYNAMIC block, we need to transmit this dictionary which
will be compressed too inside the current BLOCK. Inflator will decompress it and
be able to do the _reverse_ translation between bit sequence and byte.

Finally, on the header, we have a bit which inform the inflator if it's the last
block (and stop the process) or not.

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
available in my outputs buffer. Then, we should provide an action to _flush_
this outputs buffer. At the end, this outputs buffer should be given by the user
(how many bytes he wants to allocate to store outputs flow).

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
bytes inflator has stored in `o` and `flush` will just set `decoder` to be able
to recompute the flow.

`w` is needed about the [lz77][lz77] compression. However, as we said, we let
the user to give to us this intermediate buffer. The idea behind that is to let
the user to prepare an _inflation_. For example, in [ocaml-git][ocaml-git],
instead to allocate `w` systematically when we want to decompress a Git object,
we allocate `w` one time per threads and all are able to use it and __re-use__
it. By this way, we avoid a systemaical allocation (and allocate only once time)
which can have a serious impact about performances.

#### Deflator

The deflator is a new (complex) deal. Indeed, behind it we have two concepts:
- the encoder (according to RFC 1951)
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
get an other strategy to let the user to use his compression algorithm.

##### Split encoder and compressor

So, the choice was done to split the encoder which respects RFC 1951 and the
compressor under some constraints. However, this is not what [zlib][zlib] did
and, by this way, we decided to provide a new design/API which did not follow,
in first instance, [zlib][zlib] (or some others implementations like
[miniz][miniz]).

To be fair, the choice from [zlib][zlib] and [miniz][miniz] comes from the first
point about API and the context where they are used. The main problem is the
shared queue between the encoder and the compressor. In a C code, it can be hard
for the user to deal with it (where he is liable by a buffer overflow).

In OCaml and for `decompress`, the shared queue can be abstracted and API can
ensure assumptions (like bounds checking).

Even if this design is much more complex than before, coverage tests are better
where we can separately test the encoder and the compressor separately. It
breaks down the initial black-box where compression was intrinsec with
encoding - which was error-prone. Indeed, `decompress` had a bug about
generation of [huffman-coding][huffman-coding] but we never reached it because
the (bad) compressor was not able to produce something to get it.

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
according RFC 1951 where _offset_ can not be upper than the length of the window
(commonly 32ko). _length_ is limited too to `258` (an arbitrary choice).

Of course, about the `Await` case, the compressor comes with a `src` function as
the inflator. Then, we added some accessors, `literals` and `distances`. The
compressor does not build the [_huffman_ coding][huffman-coding] which needs
frequencies, so we need firstly to keep counters about that inside the state and
a way to get them (and pass them to the encoder).

##### The encoder

Finally, we can talk about the encoder which will take the shared queue filled
by the compressor and provide an RFC 1951 compliant outputs flow.

However, we need to talk about a special _detail_. When we want to make a
DYNAMIC block from frequencies and then encode the inputs flow, we can reach a
case where the shared queue contains an _opcode_ (a _literal_ or a _copy_) which
does not appear in our dictionary.

In fact, if we want to encode `[ Literal 'a'; Literal 'b' ]`, we will not try to
make a dictionary which will contains the 256 possibilities of a byte but we
will only make a dictionary from frequencies which contains only `'a'` and
`'b'`. By this way, we can reach a case where the queue contains an _opcode_
which can not be encoded by the _already decided_ [_huffman_
coding][huffman-coding] - remember, the DYNAMIC block __starts__ with the
dictionary.

An other point is about inputs. The encoder expects, of course, contents from
the shared queue but it wants from the user the way to encode contents: which
block we want to emit. So it as two entries:
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
  current block. Then, he must continue with a new `Block` operation.
  
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
behavior.

Because we decided to split encoder and compressor, the idea of the _flush mode_
does not exists anymore where the user explicitly needs to give to the encoder
what we want (make a new block? which block? keep frequencies?). So we broke the
black-box. But, as we said, it was possible mostly because we can abstract
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
the opposite is not true.

In other side, let the user to do everything is the wrong way, in any case, this
way is error-prone.

So the best advise about how to write a library is keep in your mind what you
__really__ want - and you will discover step by step the real issue, and, by
this way, the real reason of your work.

## Optimization

First at all, optimisation should be done at the end of the developement
process. As @Drup said, first you must have something which works, then you can
think about optimization. In fact, an optimization pass can change a lot your
code, so you need to keep a state of your project where you can trust on it.
This is assumption will help you as a comparison point about benchmark first,
and certainly about expected behaviors.

In others words, your stable implementation will be the oracle for your
optimization. So if you start from nothing, you probably will do everything, but
surely anything.

Before to start to talk about optimization, keep in your mind this rule. We did
optimizations on `decompress` because, for a long time (2 years), we use it in
bigger projects. So we have an oracle (even if `zlib` can be the oracle in this
special case).

### Specialization

One of the biggest specialization on `decompress` is about the `min` function.
If you don't know, in OCaml `min` is polymorphic - you can have an order of
anything. So you probably have some concerns about how we implemented `min`?

And you are right, if you go to details, at the end, `min` will call
`do_compare_val`, a C code which traverse your structure and do a comparison
according the type (in terms of the runtime) of your structure.

Of course, for integers, it shoud be only a `cmpq` assembly instruction.
However, a simple code like:

```ocaml
let x = min 0 1
```

will produce this CMM and assembly code:

```cmm
(let x/1002 (app{main.ml:1,8-15} "camlStdlib__min_1028" 1 3 val)
   ...)
```

```asm
.L101:
        movq    $3, %rbx
        movq    $1, %rax
        call    camlStdlib__min_1028@PLT
```

Not that _beta-reduction_, _inlining_ or specialization was not done in this
code. OCaml does not optimize too much your code - the good point is
predictability of the produced assembly output.

So if you help a little bit the compiler with:

```ocaml
external ( <= ) : int -> int -> bool = "%lessequal"
let min (a:int) b = if a <= b then a else b

let x = min 0 1
```

We have:

```cmm
(function{main.ml:2,8-43} camlMain__min_1003 (a/1004: val b/1005: val)
 (if (<= a/1004 b/1005) a/1004 b/1005))
 
(function camlMain__entry ()
 (let x/1006 1 (store val(root-init) (+a "camlMain" 8) 1)) 1a)
```

```asm
camlMain__min_1003:
        .cfi_startproc
.L101:
        cmpq    %rbx, %rax
        jg      .L100
        ret
```

So we have all optimizations, in this produced code, `x` was evaluated as `0`
(`let x/... (store ... 1)`) (_beta-reduction_ and _inlining_) and `min` was
specialized to accept only integers - so we are able to emit `cmpq`.

#### Results

From this specialization, we won 10 Mb/s about decompression where `min` is used
in several places. We completely avoid an indirection and a call to
`do_compare_val` which is really slow.

I would like to say that this kind of specialization is already done by
`flambda`, however, we currently use OCaml 4.07.1. So we decided to do by
ourself this kind of optimization.

The reason about what we need to redefine the primitive `lessequal` is little
bit annoying and not very true for the last version of OCaml - it's certainly
boring and not very useful at the end. But this kind of optimization is
error-prone where we can mistap `<=`.

### Inlining

In the first example, we showed a code with the `[@@inline]` keyword which is
useful to force the compiler to inline a little function. We will go outside the
OCaml world and enter to the C code (gcc 5.4.0) to really understand the
_inlining_.

In fact, _inlining_ is not necessary the best optimization.

```c
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>

#ifdef HIDE_ALIGNEMENT
__attribute__((noinline, noclone))
#endif
void *
hide(void * p) { return p; }

int main(int ac, const char *av[])
{
  char *s = calloc(1 << 20, 1);
  s = hide(s);

  memset(s, 'B', 100000);

  clock_t start = clock();

  for (int i = 0; i < 1280000; ++i)
    s[strlen(s)] = 'A';

  clock_t end = clock();

  printf("%lld\n", (long long) (end-start));

  return 0;
}
```

We can take this code which does not have any sense but deserve our purpose.
Indeed, we can imagine to compile this code with `-O2` (the second level of
optimization in C). However, we will generate two outputs, one with
`-DHIDE_ALIGNEMENT` and an other without.

Assembly emitted differs:

```asm
.L3:
	movq	%rbp, %rdi
	call	strlen
	subl	$1, %ebx
	movb	$65, 0(%rbp,%rax)
	jne	.L3
```

```asm
.L3:
	movl	(%rdx), %ecx
	addq	$4, %rdx
	leal	-16843009(%rcx), %eax
	notl	%ecx
	andl	%ecx, %eax
	andl	$-2139062144, %eax
	je	.L3
```

In this the first output (with `-DHIDE_ALIGNEMENT`), the optimization pass
decides to disable _inlining_ on `strlen`, in the second output (without
`-DHIDE_AlIGNEMENT`), it decides to inline `strlen` (and do some others clever
optimizations).

The reason behind this complex behavior from the compiler is clearly described
[here][gcc-strlen].

But what we want to say is that _inlining_ is __not__ an automatic optimization
and can produce, at the end, a _pessimization_. This is the goal of `flambda`,
do the right optimization under the right context. And if you are really curious
about what `gcc` does and why, even if it's very interesting, the reverse
engineering of the optimization process and which information is relevant about
the choice to optimize or not is deep, long and surely too complicated.

A non spontaneous optimization is to annote some parts of your code with
`[@@inline never]` - so, explicitly say to the compiler to not inline the
function. This constraint is to help the compiler to generate a smaller code
which will have more chance to fit under the processor cache.

For all of these reasons, `[@@inline]` should be used sparingly and an oracle to
compare performances if you inline or not this or this function is necessary to
avoid a _pessimization_.

#### In `decompress`

The in `decompress` was done about some small functions which need to allocate
to return a value. If we inline them, we can take the opportunity to store
returned value in registers (of course, it depends how many registers are free).

As we said, the goal of the inflator is to translate a bit sequence to a byte.
The larger bit sequence possible according RFC 1951 has the length 15. So, when
we process an inputs flow, we eat it 15 bits per 15 bits. For each packet, we
want to recognize an existing associated bit sequence and then, binded values
will be the real length of the bit sequence and the byte:

```ocaml
val find : bits:int -> { len: int; byte: int; }
```

So for each call to this function, we need to allocate a record/tuple. So it's
why we choose to inline this function. `min` was inlined too and some others
small functions. But as we said, the deal is complex and where we think that an
_inlining_ can help us, it's not systematically true.

### _Tagged-integer_

In some assembly outputs when we talk about `0` integer, we can see `$1` in
assembly. It's about the [GC bit][jst-gc-bit] needed to differentiate a pointer
and an unboxed integer. This is why, in OCaml, we talk about a 31-bits integer
or a 63-bits integer (depending your architecture).

We will not try to start a debate about this arbitrary choice on the
representation of an integer in OCaml. However, we can talk about some
operations which can have an impact on performances.

The biggest example is about the `mod` operation. Between OCaml and C, `%` or
`mod` should be the same:

```ocaml
let () =
  let a = int_of_string Sys.argv.(1) in
  let b = int_of_string Sys.argv.(2) in
  let x = ref 0 in
  
  for _ = 0 to max_int do
    x := a mod b
  done ;
  
  print_int !x
```

The output assembly is:

```asm
.L105:
        movq    %rdi, %rcx
        sarq    $1, %rcx
        movq    (%rsp), %rax
        sarq    $1, %rax
        testq   %rcx, %rcx
        je      .L107
        cqto
        idivq   %rcx
        jmp     .L106
.L107:
        movq    caml_backtrace_pos@GOTPCREL(%rip), %rax
        xorq    %rbx, %rbx
        movl    %ebx, (%rax)
        movq    caml_exn_Division_by_zero@GOTPCREL(%rip), %rax
        call    caml_raise_exn@PLT
.L106:
        salq    $1, %rdx
        incq    %rdx
        movq    %rbx, %rax
```

where a C code produce:

```asm
.L2:
        movl    -12(%rbp), %eax
        cltd
        idivl   -8(%rbp)
        movl    %edx, -4(%rbp)
```

Of course, we can notice firstly the exception in OCaml (`Divided_by_zero`).
Then, we need to _untag_ `a` and `b` with `sarq` assembly operation. We do, as
the C code, `idiv` and then we must _retag_ returned value `x` with `salq` and
`incq`.

So in some parts, it should be more interesting to use `Nativeint`. However, by
default, a `nativeint` is boxed. _boxed_ means that the value is localized in
your heap and surrounded by an header.

Of course, this is not what we want so, if our `nativeint ref` (to have
side-effect, like `x`) stay inside a function and then, you return the real
value with the deref `!` operator, OCaml, by a good planet alignement, can
directly use registers and real integers. So it should be possible to avoid
these needed conversions.

#### Readability versus performance

We localize this optimization only in few parts of the code. In fact, switch
between `int` and `nativeint` is little bit noisy:

```ocaml
hold := Nativeint.logor !hold Nativeint.(shift_left (of_int (unsafe_get_uint8 d.i !i_pos)) !bits)
```

And, at the end, about the inflator, we won 0.5Mb/s so, it's not very relevant
to do systematically this optimization. Especialy that tha gain is not very
big. But this case show a more higher problem: readability.

In fact, we can optimize more and more a code (OCaml or C) but we lost, step by
step, readability. You should be afraid by the implementation of `strlen` for
example. At the end, lost readability is error-prone where is more hard (for
another guy or for you ten years after) to contextualize the reason of the code.

And we think that this kind of optimization is not the way of OCaml in general
where we prefer to produce an understanble and abstracted code than a cryptic
and super fast one.

### Exception

### Unroll

### _hot-loop_

### Conclusion

## `decompress` at the end
