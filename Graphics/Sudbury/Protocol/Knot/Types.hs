{-|
Module      : Graphics.Sudbury.Protocol.Knot.Types
Description : Knotted protocol data: interfaces whose messages' arguments refer to other interfaces
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.Knot.Types where

import Data.Fix

import Graphics.Sudbury.Protocol.XML.Types

type KnotInterface = Fix (Interface ArgEnum)
type KnotMessage = Message ArgEnum KnotInterface
type KnotArgument = Argument ArgEnum KnotInterface
type KnotProtocol = Protocol ArgEnum KnotInterface
