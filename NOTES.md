These are some of my notes in an attempt to understand wayland/libwayland

Overview
===
libayland is a library written in C that implements the wayland protocol. The library has two variants, one which is linked to by the client (which typically e.g. wants to draw a window), and one which is linked to by the server (aka compositor). Essentially the only job of the wayland protocol is to pass messages and set up shared memory buffers. The messages can contain events and requests like:
- "the mouse just moved to position this and that"
- "can you please put this window over there?" 
- "the user just plugged in another monitor with such and so dimensions"

The shared memory buffers are used to exchange bulk data, usually screen drawings. Memory buffers are shared by sending a message that contains an `fd` that maps to a piece of RAM.

All messages are sent by either the server to the client (this is what wayland calls an "event"), or by the client to the server (known as a "request"). I emphasize this because requests might actually be messages such as "I just finished drawing in this buffer", and events might be messages such as "please adjust the dimensions of your window". So this terminology can become a bit confusing.

A message is always bound to a wayland "object". The identifying data of an object is an ID that is communicated at creation time. During their lifetime, objects have no other properties, although you may wish to view its associated messages as configuring its state - but wayland is oblivious to this. We typically associate the object's ID to a part of the internal state of the compositor.

Other documentation:

- [Official wayland docs](http://wayland.freedesktop.org/docs/html/)
- [The Real Story Behind Wayland and X (presentation)](https://www.youtube.com/watch?v=RIctzAQOe44)
- [Pekka Paalanen's object lifespan blog post](http://ppaalanen.blogspot.com/2014/07/wayland-protocol-design-object-lifespan.html)
- [Jan Newmarch's Programming Wayland Clients](https://jan.newmarch.name/Wayland/)

Terminology
===
- Event: a short message that's sent from the server to the client
- Request: a short message that's sent from the client to the server
- Message: either an event or a request. Always associated to an object (so either a proxy or a resource)
- Interface: Every object gets a type, and in wayland we call these types interfaces. here we understand the word "interface" as in "Application Programming Interface": the various interfaces give access to the different parts of the compositor. The complete specification of the interfaces is in the XML protocol.
- Protocol: libwayland's API consists of a relatively small set of semi-fixed utility functions (wayland-client.h, wayland-server.h, wayland-util.h, wayland-egl.h, wayland-version.h). The remainder is specified by a list of interfaces that one can interact with via the wayland protocol, specified by XML files. E.g., there is a "wl\_pointer" interface with a "motion" event with a "time" argument, and "surface\_x" and "surface\_y" coordinate arguments. The wl\_pointer represents a pointer device, and the motion event signals that its position changed. Objects of type wl\_surface are drawing surfaces, wl\_keyboard represent physical keyboards. And so forth.

  A protocol is specified by a list of interfaces. An interface is specified by a list of events and requests. A message is specified by a list of arguments. Arguments are primarily defined by a name and a type (e.g. integers, byte strings, or other objects). When sending a message, you have to specify which object to send a message on (by passing its ID), and the value of the arguments.
  
  Several protocols may be used on the same connection. An object of interface wl\_registry is used to negotiate which interfaces are used. The server offers a list of interfaces to the client, and then the client can choose to make use of them or not.
- Proxy: the client-side representation of a wayland object
- Resource: the server-side representation of a wayland object, but also used to refer to the object that both sides have handles to (ie. as a synonym for "wayland object")
- Enum: a list of constants. Used to give semantics to integer message arguments. Sometimes actually an enumeration, sometimes a bitfield (meaning several constants may be summed to give a new valid constant), sometimes neither.
- Argument: messages can have several bits of data associated with them, which are called arguments.
- Display: any of the following: the actual physical display, or a wl\_display, which has three strictly distinct flavors: a server side resource, a client side proxy, or a client side C struct which has a proxy as its first member whose interface is wl_display, and which is therefore usable as a proxy (the third option is the one that is returned by wl\_display\_connect)
- Scanner: A piece of code that generates an API (for C, this would be wayland-server-protocol.h, wayland-client-protocol.h) from the protocol files (wayland.xml and others).

Object creation
===
Suppose you want to send or receive messages for some interface. Since all messages are associated to an object, you have to first construct the object for which you want to send or receive messages.
Objects are constructed by allocating an ID, and informing the other side that an object of that type was just created. The client is allowed to use IDs starting from 0x0000001, and the server is allowed to use IDs starting from 0xff000001. (TODO: isn't there an off-by-one error here? And what is the maximum number of IDs?)
In either case, when one of the sides constructs an object, they inform the other side of this new object.

There is a dependency problem: how do you request your first object? There is no object to use as a channel to inform the other side of its creation.
This is what the display (wl\_display) object is for: it is an singleton object which you (implicitly) create with fixed ID 1 when you connect to a server.
Once you have a wl\_display, the client can proceed to request the wl\_registry, which has a "bind" request that can construct objects that have a "global" interface (as indicated by the "global" event).

wl\_display and wl\_registry are the only two protocol interfaces that have a sort of elevated status above the other interfaces: wl\_display is created via a process called "connecting to a server", and a wl\_registry is a sort of superinterface to create a variety of other objects.

As long as the client follows the protocol (ie. only construct objects which the server allowed it to construct), object creation is guaranteed to work: so the client does not need to wait for the server to verify object creation, and can just continue buffering messages on freshly created objects without sending any data. In this sense wayland is an asynchronous protocol.

The client and server can both destroy objects: the client using wl\_proxy\_destroy, and the server using wl\_resource\_destroy.
Additionally, some interfaces expose a "destroy" request, which, if available, must be used.
This informs the other side that the object is no longer of interest to them.

Sending messages
===
Every message has a list of arguments. Every argument has a name and a type. The following types are available:

- int, uint, fixed (integer, unsigned integer, fixed-point number)
- object, new\_id
- string, array (not such great names as the data that is sent on such arguments are binary blobs from wayland's point of view)
- fd (file descriptors)

Most types speak for themselves, but the "object" and "new\_id" types need some discussion.

Most arguments are sent in the direction of the message itself. However, the new\_id type is used to create objects: so a client can get access to an object by sending a request which has a new_id type argument.

We imagine objects as existing on the server side - the wayland protocol simply provides an interface to them for the client. You can get and set their attributes using the various requests and events.

When a request has a new\_id argument, the request instructs the server to create an object with a given identification.
Such creations are guaranteed to work as long as they are requested in the right way: e.g. you can't cheat the registry to create a non-global interface object for you, as this is considered an error.
So this gives the client a handle to an object on the server.
So we can imagine objects flowing against the flow of the message here: technically the client just sends an instruction to the server to create an object, and assumes it worked, but it kind of looks like the client "gets" an object from the server.

One might wonder about `fd` type arguments: you can't just pass the identifying number of an open file. Indeed, the wayland protocol depends on a very specific extension of unix domain sockets to pass `fd`s over it. See the control message code in `cbits/wayland-msg-handling.c`.

Various quotes from #wayland IRC
===

    <tulcod> "As all requests and events are always part of some interface (like a member of a class), this creates an interface hierarchy. For example, wl_compositor objects are created from wl_registry, and wl_surface objects are created from wl_compositor."
    <tulcod> so what if i use wl_registry.bind to create a wl_surface?
    <giucam> tulcod: i'm not sure what will happen exactly but it won't work, as there is no wl_surface global. probably you'll get a protocol error
    <tulcod> giucam: but "Object creation never fails."
    <giucam> tulcod: i don't think that applies to globals
    <giucam> binding a global can fail, and the server will create a dummy wl_resource, send an error and destroy it
    <tulcod> ah, so it can fail, it's just that it tells you afterwards
    <tulcod> ie it tells you if it failed, or doesn't tell you anything if it succeeded
    <giucam> but i'm not sure what wl_registry.bind returns if it fails
    <giucam> i.e if it is a valid proxy, and what happens if you call a request on it
    <giucam> probably fails in a non-destructive way


    <pq> tulcod, all requests always "succeed" - if they don't, you violated the protocol.


    <pq> tulcod, did you already notice that there are at least three different wl_display structs?
    <pq> server-side wl_display (opaque), client-side wl_display (opaque), and the wl_proxy cast to wl_display IIRC
    <pq> the first member of client wl_display is a wl_proxy
    <pq> so it's interchangeable


    <pq> tulcod, btw. another thing you should be wary with is wl_buffer.
    <pq> tulcod, again, the client-side wl_buffer is a wl_proxy. The server-side is the tricky one.
    <tulcod> pq: ah, but the server-side one is deprecated :)


    <Cloudef> I wonder if wl_resource_destroy_func_t is needed for set_implementation calls which interface already contains destroy function
    <jekstrand> yes, they serve different purposes
    <jekstrand> The one on the interface lets you know that the client has sent the destroy request
    <jekstrand> The wl_resource_destroy_func_t lets you know that the object has been destroyed and lets you clean stuff up.  This happens to all the resources when the client disconnects.
    <tulcod> Cloudef: you mean "interfaces which have a 'destroy' request" ?
    <Cloudef> tulcod: yes
    <tulcod> ok yes, then i agree with jekstrand
    <Cloudef> jekstrand: on which function I should actually free the resource?
    <Cloudef> weston seems to do this on the interface one
    <Cloudef> (and on the other one too)
    <tulcod> (fwiw, it's no problem to free twice)
    <jekstrand> Cloudef: You need to call wl_resource_destroy in the interface one.  wl_resource_destroy calls th wl_resource_destroy_func_t one.
    <jekstrand> Cloudef: If you're familiar with C++, wl_resource_destroy is like "delete res" whereas the wl_resource_destroy_func_t is like MyThing::~MyThing()
    <pq> why those two are separate is because wl_resource_destroy may end up called also at other times, namely when cleaning up state after a client disconnection.
    <jekstrand> the interface one is just a request, that's all.  there's no implicit destruction of resources.
    <pq> ..except on the client side, so the server side cannot really not do it, too
    <pq> since server and client must agree on what objects exist
    <jekstrand> yup


    <pq> when we say that an object is a singleton, we usually mean the real object, not the protocol object


    <tulcod> jekstrand: uhm, so let's suppose the client has a wl_shell_surface
    <tulcod> jekstrand: and the server, for whatever reason, destroys it
    <tulcod> what am i missing? can't the client keep the handle to that non-existent object?
    <jekstrand> the server can't just up and destroy it
    <jekstrand> that's a broken server.  Clients shouldn't handle that case.
    <tulcod> jekstrand: okay, so compositors can't freely destroy stuff?
    <jekstrand> nope
    <jekstrand> If compositors were allowed to add-hock destroy client's objects, that would result in insanity


    <jekstrand> RE: Object destruction:  A better way to think about the object lifecycle is that the client manages (creates/destroys) objects.  The special cases of server-destroyed objects isn't so much the server destroying things as objects getting "automatically" destroyed because it's clear that they're no longer needed (such as when a wl_callback is finished.) The server simply storres them.

wl_resource.link
===
used internally for the registry.
exposed for other usage.
wl_list links need a real C struct, so we must package resources into them.

libffi
===
used for all callbacks from libwayland to C code (see wl_closure_invoke)

display->mutex
===
libwayland's big global lock.
