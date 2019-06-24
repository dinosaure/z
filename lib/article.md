# Decompress, or how to implement a systemic library in OCaml

You probably have `zlib` installed in your computer but you don't know where it
comes from. You can see it when you start your computer and where your Linux
kernel inflates by itself. Your Git objects are compressed with it too and,
according RFC 1952 and HTTP 1.1, you probably got this page as an GZIP stream.

## `decompress`

`decompress` wants to provide a way to compress/uncompress a stream and where
most of the code is in OCaml, specially compression algorithm, encoder and
decoder. It implements [RFC1951][rfc1951] and rely on the OCaml runtime instead
of the C runtime.

Of course, this project is under the MirageOS project as a layer of
[`ocaml-git`][ocaml-git] for example. In this context, it's tricky (and risky)
to link with an external library like `zlib` where:

- most of _syscalls_ used in are not available in the KVM target for example
- [CVE][cve] can exist in your `zlib` library (which is outside the scope of the
  OCaml ecosystem)
- C code, even if it's a well tested code, can be considered as dangerous code

For all of these reasons, we should let the end-user to choose its
implementation and the first purpose of `decompress` is to deliver this choice
(not to supplant `zlib`).

### As an OCaml project

`decompress` is really interesting where we want to implement a library which
should be fast and reliable. Deal with both is hard where performance can bring
undefined behaviors, bugs and, by this way, make it hazardous. In other side,
systematic checks and assertions can slow down the process make it unusable in a
industrial environment.

This deal reveals some others problematics like readability where we want to fit
under the compiler logic or compatibility where we start to compare
implementations.

In this article, we will show you all of theses and announce the first **major**
release of `decompress`.

### API

As a library provider, one of the biggest work is about API or Application
Programming Interface. This work will impose how to use library, and, under this
question, we have several concerns:

- An API should restrict the user where we have some security concerns
- An API should be used in different contexts (in a fancy-and-dirty tool or a server)
- An API should prompt end-user to a good design-pattern
- An API should be fully understable (even if it is complex)

To fully answer to these problematics, we need several iterations[^1] where only
usability show you these problems. Of course, `decompress` was under this logic
where it currently has nine versions.

[1]: And this it why the best way to participate to the MirageOS project is to share your experience with our libraries.

#### A Stream API

One big question is: how to represent an compress/uncompress stream? `camlzip`
decided to represent it as:

- a *refiller* function: `bytes -> int`
- a *flusher* function: `bytes -> int -> unit`

A big lack of this representation is error handler where it's not explicitly
described on types and it's why it uses as a side output exceptions. So, any use
of `camlzip` should be wrapped in a `try .. with`.

Of course, API does not enforce to end-user this design.

An other aspect of this API is the allocation of the `bytes` given to the user
under the *flusher* and the *refiller*. Where it comes? Can we decide which
length it has? Can it fit under _syscall_ limitations? Can we reuse it?

Most of these questions is about a control in memory of inflate/deflate
computation. In a *64Gb in RAM* context, it should not be a problem but in an
[ESP32][esp32] target, it is (even if `camlzip` [allocates][camlzip-allocate] a
1024-bytes buffer).

So user will not be able to decide these details (where it can be relevant).

Then, for each call of `compress`/`uncompress`, you do the allocation where we
probably can reuse internal buffers (like the [window][window-allocate]) -
however, in a LWT/Async context, this ability is error-prone where we can easily
violate the ownership into several processes.

#### Input/Output logic

In MirageOS, we are aware about Input/Output logic where `read` or `write` can
come from several implementations. So, at any times, we should not take the
advantage of a specific _syscall_ and completely be abstracted to where come
from inputs and where we should send outputs.

Amazingly, `zlib` follows the same logic where end-user needs to do the glue
between the inflater and _syscalls_. It's mostly because `zlib` is a layer of
some others specifications (like PACK file in Git) and we should be able to
compose `zlib` with other layers/protocols.

`ocaml-tls` was made with the same design where we should _wrap_ HTTP in TLS
for example.

This composition idea is not so easy where `camlzip` expects an `in_channel`
where it processes a `gzip` format for example. Then, it expects the idea of a
file-descriptor where, in a MirageOS context, we should not have this kind of
things - mostly because we can make an unikernel without a file-system.

At the end, `decompress` wants to provide an API which only needs buffers. Then,
end-user will play with them to directly deal with `input : ic -> bytes -> off:int -> len:int -> int`
and `output : oc -> bytes -> off:int -> len:int -> unit` or share them with
another decoder (like a [TAR][tar] decoder) to compose streams.

#### A *dbuenzli* interface

@dbuenzli did several libraries with a pretty common interface to consume and
produce a stream like a [JSON][jsonm] stream or an [Unicode][uutf] stream. At
the beginning, `decompress` wants to follow this kind of API but internally it
decides to deal with an immutable state since v0.4 instead of a mutable state -
like [`ocaml-tls`][ocaml-tls].

Interface looks like:

```ocaml
val decode : state -> [ `Await | `Flush | `End | `Malformed of string ]
```

A computation `decode` returns 4 cases which are easily understable:
- `Await` means that the current state expect more inputs
- `Flush` means that internal buffer of state is full and need to be flushed
- `End` to signal end of computation
- `Malformed` if the state reach invalid contents

If we compare with `camlzip`, `Await` and `Flush` are `refiller` and `flusher`.
Despite to define functions, we use pattern-matching, and, by this way, let the
end-user to do everything even if he wants to left out process.

This kind of API lets a biggest control about the control flow if we compare
with `camlzip` where we should write non-conventional code to keep in our
control entirely the process (like simulate a jump with exception).

Another good point is we can reach the `camlzip`'s API with easily:

```ocaml
let compress refiller flusher =
  let tmp_i = Bytes.create 1024 in
  let tmp_o = Bytes.create 1024 in
  let decoder = decoder `Manual ~o:tmp_o in

  let rec go () = match decode decoder with
    | `Await ->
      let len = refiller tmp_i len in
      src decoder tmp_i 0 len ; go ()
    | `Flush ->
      flusher tmp_o (1024 - dst_rem decoder) ;
      flush decoder ; go ()
    | `End -> flusher tmp_o (1024 - dst_rem decoder)
    | `Malformed err -> invalid_arg err in
  go ()
```

With this snippet we keep the same interface than `camlzip` and we can see that
internaly, we do several stuffs like allocation. `zlib` (in C) are very close to
what `decompress` does and C stubs needed by `camlzip` do closely the same.

Finally, the ability of us to do allocation and initialization in OCaml is
better than in C where we mostly don't want to allocate anything in C - and take
the advantage of `[@@noalloc]`. Generally less C code is better!

---

We can notice some others functions like `dst_rem` or `flush`. They are mostly
accessors or setters with the `decoder` state. At the end, we reach the second
point about API where we need to initialize `decoder` state with our buffer -
and fit under hostile context.

#### Fine-grained interface

It should be hard to see the limit between what we can provide and what we
should hide. Currently, `zlib` did the choice to hide the compression algorithm.
So we are not able to have something else than [LZ77][lz77] or [LZWA][lzwa].

The problem with this way is to lost the control about what we can generate
between the compressor algorithm and the encoding process. We should be able,
just for a coverage problem, to encode anything (even if it's stupid).

This new version of `decompress` comes with this new mind where the previous API
does not allow to use something else than the pretty stupid implementation of
the LZ77 algorithm.

Then, between the compression algorithm and the encoding process, as a stream,
we have a *weighted* queue (like [ke][ke]).
