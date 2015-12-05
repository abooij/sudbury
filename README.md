Sudbury
===
Previously, I wrote Haskell bindings to the C wayland library. But these are ugly and are not able to satisfy every need.

After thinking about this problem for a while, I realized that this is ultimately due to the nature of the libwayland API: it is simply not very flexible. This is reflected by the fact that there are many wayland utilities (e.g. middle-man protocol dumpers) that simply end up copy-pasting a bunch of wayland implementation code.

Surely we can do better.

This is an attempt at creating an elegant wayland library that will serve many purposes: desktop applications, compositors, protocol dumpers, test utilities, ...

In addition, I want to support many different programming paradigms - especially since in Haskell there are many different approaches to streaming IO (lazy IO, conduit, pipes, io-streams, ...) and event handling (polling, callbacks, FRP, ...), which I would all like to support equally well.

In case you were wondering if Sudbury might be a town close to Wayland, MA: you are right. I'm not good in naming things. Deal with it.
