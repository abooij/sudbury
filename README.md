Sudbury
===
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
The package should be buildable using [cabal](https://github.com/haskell/cabal) (which is available as a package in many Linux repositories) or [stack](http://haskellstack.org/) (which is a newer project aimed at avoiding common Haskell installation pitfalls). (If you don't know which build tool to pick, give stack a try, as it should give reproducible builds.)

To build with cabal, clone the repository and run
```
$ cabal sandbox init
$ cabal install
```
(It is installed locally in a subdirectory of the repository.)

To build with stack, (first setup stack) clone the repository and run
```
$ stack build
```
(The binary is again placed in a subdirectory.)

Note that the `libwayland-client.so.0` shared library will __not__ be installed in the correct location.
This is because the build suite, cabal, believes we are compiling this library as an executable.
There are developments to allow building platform libraries (as we are doing) without too many hacks.

If you built the project with stack (which internally uses parts of cabal), you can use it as follows:
```
$ LD_LIBRARY_PATH=.stack-work/install/x86_64-linux/lts-7.0/8.0.1/bin weston-flower
```

If you built the project with cabal, you can use it as follows:
```
$ LD_LIBRARY_PATH=.:dist/build weston-flower
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

Status (September 2016)
---
So far, the main focus is implementing a C ABI for the client side.
As of commit 24e5b89, all weston demo clients run correctly (but please report any clients that work with libwayland but not with sudbury).
The Haskell API is still rather limited (see issue #2 for progress), and the server side ABI has not yet been written (see issue #3 for progress).

This all means that you should be able to use sudbury as a drop-in replacement for wayland clients, but not yet for wayland compositors. And while some code is in place to be able to write wayland programs in Haskell, much more functionality is planned.

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
