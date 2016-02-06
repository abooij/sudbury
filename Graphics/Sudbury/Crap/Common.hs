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

import Data.Word
import Control.Concurrent.STM
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types

import Graphics.Sudbury.Argument

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

charToArgType :: CChar -> ArgTypeBox
charToArgType c =
  case castCCharToChar c of
       'i' -> ArgTypeBox SIntWAT
       'u' -> ArgTypeBox SUIntWAT
       'f' -> ArgTypeBox SFixedWAT
       'o' -> ArgTypeBox SObjectWAT
       'n' -> ArgTypeBox SNewIdWAT
       'a' -> ArgTypeBox SArrayWAT
       'h' -> ArgTypeBox SFdWAT
       o   -> error ("Unexpected argument '" ++ [o] ++ "' encountered")

generateId :: TVar Word32 -> TVar [Word32] -> STM Word32
generateId lastId avail = do
  free <- readTVar avail
  case free of
    (x:xs) -> do
      writeTVar avail xs
      return x
    [] -> do
      n <- readTVar lastId
      writeTVar lastId (n+1)
      return (n+1)

returnId :: Word32 -> TVar [Word32] -> STM ()
returnId pid avail = modifyTVar avail (pid:)

type family CArgument (t :: ArgumentType) where
  CArgument 'IntWAT = CInt
  CArgument 'UIntWAT = CUInt
  CArgument 'FixedWAT = CInt
  CArgument 'StringWAT = CString
  CArgument 'ObjectWAT = Ptr ()
  CArgument 'NewIdWAT = CUInt
  CArgument 'ArrayWAT = Ptr WL_array
  CArgument 'FdWAT = Fd

data CArgBox = forall t. CArgBox (SArgumentType t) (CArgument t)
