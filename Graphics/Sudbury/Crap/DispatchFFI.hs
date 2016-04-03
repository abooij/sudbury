{-|
Module      : Graphics.Sudbury.Crap.DispatchFFI
Description : C API crap
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

Use libffi to call C functions for callbacks
-}
module Graphics.Sudbury.Crap.DispatchFFI where

import Foreign.Ptr
import Foreign.LibFFI
import System.Posix.Types

import Graphics.Sudbury.Argument
import Graphics.Sudbury.Crap.Common

-- See `convert_arguments_to_ffi` in connection.c in libwayland
packageCArg :: SArgumentType t -> CArgument t -> Arg
packageCArg SIntWAT    = argCInt
packageCArg SUIntWAT   = argCUInt
packageCArg SFixedWAT  = argCInt
packageCArg SStringWAT = argPtr
packageCArg SObjectWAT = argPtr
packageCArg SNewIdWAT  = argPtr
packageCArg SArrayWAT  = argPtr
packageCArg SFdWAT     = argCInt . (\(Fd x) -> x)

invokeFFI :: Implementation -> UserData -> Ptr () -> [CArgBox] -> IO ()
invokeFFI fun udata objptr args =
  callFFI fun retVoid (argPtr udata : argPtr objptr : map (\(CArgBox x a) -> packageCArg x a) args)
