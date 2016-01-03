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

import Graphics.Sudbury.Internal
import Graphics.Sudbury.Argument

type family UnboxedArgument (r :: *) (t :: ArgumentType) where
  UnboxedArgument r 'IntWAT = Int32
  UnboxedArgument r 'UIntWAT = Word32
  UnboxedArgument r 'FixedWAT = Fixed23_8
  UnboxedArgument r 'StringWAT = B.ByteString
  UnboxedArgument r 'ObjectWAT = (r , Word32)
  UnboxedArgument r 'NewIdWAT = (r , Word32)
  UnboxedArgument r 'ArrayWAT = B.ByteString
  UnboxedArgument r 'FdWAT = ()

data ArgBox r = forall t. ArgBox (SArgumentType t) (UnboxedArgument r t)

data Message r = Message
  { messageSender :: Word32
  , messageArguments :: [ArgBox r]
  }
