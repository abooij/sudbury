{-|
Module      : Graphics.Sudbury.Crap.Common
Description : C API crap
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

The wayland C API has some free crap included in it.
This implements common crap.
-}
module Graphics.Sudbury.Crap.Common where

import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types

import Graphics.Sudbury.Crap.Structs

-- | A specific implementation (ie for one opcode) is some abstract function
-- pointer. Since libffi doesn't care anyway, we'll make it something trivial.
type Implementation = FunPtr ()
-- | Listeners are structs of function pointers, so essentially (C-style) arrays
type Listener = Ptr Implementation
type UserData = Ptr ()
type DispatcherData = Ptr ()

{-
 * A dispatcher takes five arguments:  The first is the dispatcher-specific
 * implementation data associated with the target object.  The second is the
 * object on which the callback is being invoked (either wl_proxy or
 * wl_resource).  The third and fourth arguments are the opcode the wl_messsage
 * structure corresponding to the callback being emitted.  The final argument
 * is an array of arguments received from the other process via the wire
 * protocol.
typedef int (*wl_dispatcher_func_t)(const void *, void *, uint32_t,
				    const struct wl_message *,
				    union wl_argument *);
-}
-- the return value is ignored (even by libwayland)
-- | dispatcher function. we represent the second argument (wl_proxy or wl_resource)
-- by an untyped StablePtr which we can later cast to the right type. sorry.
type DispatcherFunc = FunPtr (DispatcherData -> StablePtr () -> CUInt -> Ptr WL_message -> Ptr WL_arg -> IO CInt)

type Dispatcher = (DispatcherFunc, DispatcherData)
