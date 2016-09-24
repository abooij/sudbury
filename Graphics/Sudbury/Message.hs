{-|
Module      : Graphics.Sudbury.Message
Description : Internal representation of messages
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

This module defines a wayland message type that tracks interface types
-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Sudbury.Message where

import Data.Word
import Data.Int
import qualified Data.ByteString as B
import System.Posix.Types

import Graphics.Sudbury.Internal
import Graphics.Sudbury.Argument

-- | This argument type captures all data in an argument: so compared
-- to WireArgument, it includes the fd.
type family FullArgument (t :: ArgumentType) where
  FullArgument 'IntWAT = Int32
  FullArgument 'UIntWAT = Word32
  FullArgument 'FixedWAT = Fixed23_8
  FullArgument 'StringWAT = B.ByteString
  FullArgument 'ObjectWAT = Word32
  FullArgument 'NewIdWAT = Word32
  FullArgument 'ArrayWAT = B.ByteString
  FullArgument 'FdWAT = Fd

data FullArgBox = forall t. FullArgBox (SArgumentType t) (FullArgument t)

data Message = Message
  { messageSender :: Word32
  , messageOpcode :: Word16
  , messageArguments :: [FullArgBox]
  }
