Sudbury [![Build Status on Travis CI](https://travis-ci.org/abooij/sudbury.svg?branch=master)](https://travis-ci.org/abooij/sudbury)
===
A haskell implementation of the Wayland protocol. Any implementation of the Wayland protocol needs to support _some_ C ABI calls if it wants to support EGL, so we just aim to implement the entire C ABI.

FAQ: Where's the sample code of this Haskell library?
---
At this stage, most work has gone into preliminaries and supporting the C client ABI. That means that this is a piece of really elegant Haskell code that can only really be used by C wayland clients. In other words, this project is currently a drop-in replacement of `libwayland-client.so` (as of commit 24e5b89, all weston clients seem to run correctly).

However, the aim is to write a really nice Haskell interface so that you can write all your Wayland programs in elegant Haskell. See issue #2 for progress on the usability of this code in Haskell. See issue #3 for progress on the C ABI for compositors (making this a drop-in replacement of `libwayland-server.so`, thus a replacement of the original C wayland code).

FAQ: This won't work because the libwayland C library assumes it's in charge
---
This library builds its own dynamic object (see the `foreign-library` section in `sudbury.cabal`). If you have a piece of C code which calls into C libwayland code, and it doesn't do e.g. hacky pointer arithmetic, then it will work equally well with sudbury. If not, report a bug.

In other words, this project is supposed to be an ABI-compatible drop-in replacement to the C project.

(Currently, it's client side only, because developer time is finite. But the server side should be doable as well.)

Philosophy
---
The Haskell bindings to the C wayland library are ugly and are not able to satisfy every need.
Ultimately, this seems to be due to the nature of the libwayland API: it hides as many implementation details as it can, and only exposes a minimal API, for a specific use case (namely compositors and windowed clients).

Let's develop a more friendly and inviting library.
Sudbury is an attempt to serve many usages of the wayland protocol: desktop applications, compositors, protocol dumpers, test utilities, ...

In addition, we would like to support many different programming paradigms - especially since in Haskell there are many different approaches to streaming IO (lazy IO, conduit, pipes, io-streams, ...) and event handling (polling, callbacks, FRP, ...).

While libwayland _hides_ implementation details, sudbury _exposes_ implementation details.
To that end, as a rule of thumb, the haskell modules in this library expose all the (top-level) variables that they define.
In particular, we minimize the amount of code that is considered "internal".
Indeed, the philosophy is that we should not _decide_ what API should be used, but merely _offer_ users a safe API by making that component the most friendly and inviting API.

We intend to place unsafe code in self-contained modules, separated from the Haskell code we intend users to use directly.

Building and running
---
The package should be buildable using a development version (i.e. >=1.25) of [cabal](https://github.com/haskell/cabal) (development version as of 27 November 2016).
As the moment, 27 November 2016, it does not seem to be buildable using [stack](http://haskellstack.org/).

To build with cabal, clone this repository and run
```
$ cabal sandbox init
$ cabal install
```
(It is installed locally in a subdirectory of the repository.)

If you installed the project with cabal, you can use it directly. Fire up your favorite wayland compositor, and use cabal exec to run existing executables with the sudbury implementation of the wayland client library:
```
$ cabal exec weston-flower
$ cabal exec weston-dnd
```

> Portability: The wayland protocol works via unix domain sockets that support passing file descriptors. That means that even if this builds on non-Linux systems, it will likely not work.

(Planned) features
---
- Implement the libwayland ABI to support running existing wayland applications without modification.
  - client-side (mostly done)
  - server-side (not started yet)
- Write a flexible Haskell API.
  - We can use inspiration from the implementation of the ABI to see which components should be exposed and how.
  - The wayland objects all have a corresponding "interface" which is much like a type. Hence, we should use some fancy Haskell features to make the API more strongly typed than libwayland's.
- Organise the code so that we can do extensive testing and benchmarking.
  - Because of the functional coding style, we will be able to test many more aspects of our code than libwayland.

Contributing
---
Most things that require attention should be documented somewhere: either in this readme file, or as an issue on github.

Any feedback on your experience using this library is welcome. Usage here encompasses:
- simply building the library on system X with build system Y
- running a wayland client using the instructions above:
  - are you able to make it use the sudbury implementation?
  - does the client connect?
  - does the client function as it does with libwayland?

For contributing code, you will probably want to get a bit more familiar with wayland. Some notes on understanding wayland are in NOTES.md. To work on:
- work on any of the outstanding issues.
- use sudbury to write tools to e.g. query wayland compositors, or to debug wayland connections. can we rewrite existing [debug tools](https://wayland.freedesktop.org/extras.html) into haskell, using fewer lines of code? can we rewrite the weston client demos?

Related projects
---
- [hayland](https://github.com/abooij/haskell-wayland): haskell bindings to libwayland (deprecated)
- [wayland-wire](https://github.com/sivertb/wayland-wire): alternative haskell implementation of the wayland protocol
- [wayland-tracker](https://github.com/01org/wayland-tracker/): haskell utility for tracking what happens between a wayland client and a wayland compositor
- [haskell-wlc](https://github.com/dlahoti/haskell-wlc): haskell bindings to a [wayland compositor library](https://github.com/Cloudef/wlc)
