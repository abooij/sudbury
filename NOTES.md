These are some of my notes in an attempt to understand wayland/libwayland

new_id allocation
===
from 0 to 0xff000000 for client-side allocation, 0xff000000 and up for server-side.
there is a maximum number of ids.
reuse removed ids using a stack algorithm.

wl_callback deadlock
===
when one thread constructs a wl_callback, it might have already returned before it is able to configure an implementation, deadlocking that thread (or something bad)

wl_shm_buffer_create
===
deprecated and "return NULL;" so unfunctional. but not deleted for stupid reasons.

wl_resource.link
===
used internally for the registry
exposed for other usage
wl_list links need a real C struct, so we must package resources into them.

libffi
===
used for *all* callbacks (see wl_closure_invoke)

display->mutex
===
big evil global lock. not always used correctly (e.g. missing for many getters and setters) in libwayland.
