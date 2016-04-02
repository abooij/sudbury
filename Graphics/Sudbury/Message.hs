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

type family UnboxedArgument (t :: ArgumentType) where
  UnboxedArgument 'IntWAT = Int32
  UnboxedArgument 'UIntWAT = Word32
  UnboxedArgument 'FixedWAT = Fixed23_8
  UnboxedArgument 'StringWAT = B.ByteString
  UnboxedArgument 'ObjectWAT = Word32
  UnboxedArgument 'NewIdWAT = Word32
  UnboxedArgument 'ArrayWAT = B.ByteString
  UnboxedArgument 'FdWAT = Fd

data ArgBox = forall t. ArgBox (SArgumentType t) (UnboxedArgument t)

data Message = Message
  { messageSender :: Word32
  , messageOpcode :: Word16
  , messageArguments :: [ArgBox]
  }
