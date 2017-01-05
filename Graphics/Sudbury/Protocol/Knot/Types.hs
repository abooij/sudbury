{-|
Module      : Graphics.Sudbury.Protocol.Knot.Types
Description : Knotted protocol data: interfaces whose messages' arguments refer to other interfaces
Copyright   : (c) Auke Booij, 2016
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.Knot.Types
  ( module Graphics.Sudbury.Protocol.Knot.Types
  , RT.ArgEnum(..)
  ) where

import Graphics.Sudbury.Argument
import qualified Graphics.Sudbury.Protocol.Runtime.Types as RT

data ArgProtDataBox = forall t. ArgProtDataBox (SArgumentType t) (ArgProtData t)

argDataProj :: ArgProtDataBox -> ArgTypeBox
argDataProj (ArgProtDataBox tp _) = ArgTypeBox tp

instance Eq (ArgProtDataBox) where
  x == y = argDataProj x == argDataProj y
instance Show (ArgProtDataBox) where
  show x = "ArgProtDataBox " ++ show (argDataProj x) ++ " [...]"

type family ArgProtData (t :: ArgumentType) where
  ArgProtData 'IntWAT = Maybe RT.ArgEnum
  ArgProtData 'UIntWAT = Maybe RT.ArgEnum
  ArgProtData 'FixedWAT = ()
  ArgProtData 'StringWAT = ()
  ArgProtData 'ObjectWAT = Maybe Interface
  ArgProtData 'NewIdWAT = Maybe Interface
  ArgProtData 'ArrayWAT = ()
  ArgProtData 'FdWAT = ()

data Protocol = Protocol
  { protocolInterfaces :: [Interface]
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName :: String
  , interfaceVersion :: Int
  , interfaceRequests :: [Message]
  , interfaceEvents :: [Message]
  , interfaceEnums :: [RT.ArgEnum]
  } deriving (Eq, Show)

data Message = Message
  { messageName :: String
  , messageArguments :: [Argument]
  , messageIsDestructor :: Bool
  , messageSince :: Int
  } deriving (Eq, Show)

data Argument = Argument
  { argumentName :: String
  , argumentType :: ArgProtDataBox
  , argumentNullable :: Bool
  , argumentSummary :: Maybe String
  } deriving (Eq, Show)
