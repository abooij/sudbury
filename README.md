Sudbury
===
Previously, I wrote Haskell bindings to the C wayland library. But these are ugly and are not able to satisfy every need.

After thinking about this problem for a while, I realized that this is ultimately due to the nature of the libwayland API: it is simply not very flexible. This is reflected by the fact that there are many wayland utilities (e.g. middle-man protocol dumpers) that simply end up copy-pasting a bunch of wayland implementation code.

Surely we can do better.

This is an attempt at creating an elegant wayland library that will serve many purposes: desktop applications, compositors, protocol dumpers, test utilities, ...

In addition, I want to support many different programming paradigms - especially since in Haskell there are many different approaches to streaming IO (lazy IO, conduit, pipes, io-streams, ...) and event handling (polling, callbacks, FRP, ...), which I would all like to support equally well.

In case you were wondering if Sudbury might be a town close to Wayland, MA: you are right. I'm not good in naming things. Deal with it.

Status (April 2016)
---
So far, my main focus is implementing a C ABI for the client side.
As of commit e3c6af6, the "weston-simple-damage" demo runs (which means we can show a ball bouncing around on the screen).
I will continue working to get clients to work, and perhaps do some work on the server side, and move on to writing a pretty Haskell API.
