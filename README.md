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
The package should be buildable using cabal or stack.

Note that the `libwayland-client.so.0` shared library will __not__ be installed in the correct location.
This is because we are, technically speaking, compiling this library as an executable.
This is because of limitations of the cabal build system.

After building sudbury, you should be able to, for example, run weston executables as follows:
```
$ LD_LIBRARY_PATH=.:dist/build weston-flower
```

> Portability: The wayland protocol works via unix domain sockets that support passing file descriptors. That means that even if this builds on non-Linux systems, it will likely not work.

Status (April 2016)
---
So far, the main focus is implementing a C ABI for the client side.
As of commit ea66bb7, most weston demo clients run correctly (see issue #5 for progress).
The Haskell API is still rather limited (see issue #2 for progress), and the server side ABI has not yet been written (see issue #3 for progress).
