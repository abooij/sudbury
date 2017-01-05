{-|
Module      : Graphics.Sudbury.Protocol.XML.Types
Description : Defines types for an XML definition of a wayland protocol
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.XML.Types
  ( module Graphics.Sudbury.Protocol.XML.Types
  , RT.Argument(..)
  , RT.ArgEnum(..)
  , RT.Entry(..)
  , RT.ArgProtDataBox(..)
  , RT.Description
  ) where

import qualified Graphics.Sudbury.Protocol.Runtime.Types as RT
{-
data ArgProtDataBox = forall t. ArgProtDataBox (SArgumentType t) (ArgProtData t)

argDataProj :: ArgProtDataBox -> ArgTypeBox
argDataProj (ArgProtDataBox tp _) = ArgTypeBox tp

instance Eq (ArgProtDataBox) where
  x == y = argDataProj x == argDataProj y
instance Show (ArgProtDataBox) where
  show x = "ArgProtDataBox " ++ show (argDataProj x) ++ " [...]"

type family ArgProtData (t :: ArgumentType) where
  ArgProtData 'IntWAT = Maybe String
  ArgProtData 'UIntWAT = Maybe String
  ArgProtData 'FixedWAT = ()
  ArgProtData 'StringWAT = ()
  ArgProtData 'ObjectWAT = Maybe String
  ArgProtData 'NewIdWAT = Maybe String
  ArgProtData 'ArrayWAT = ()
  ArgProtData 'FdWAT = ()
-}

data Protocol = Protocol
  { protocolName :: String
  , protocolDescription :: Maybe RT.Description
  , protocolInterfaces :: [Interface]
  , protocolCopyright :: Maybe String
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName :: String
  , interfaceDescription :: Maybe RT.Description
  , interfaceVersion :: Int
  , interfaceRequests :: [Message]
  , interfaceEvents :: [Message]
  , interfaceEnums :: [RT.ArgEnum]
  } deriving (Eq, Show)

data Message = Message
  { messageName :: String
  , messageArguments :: [RT.Argument]
  , messageIsDestructor :: Bool
  , messageSince :: Int
  , messageDescription :: Maybe RT.Description
  } deriving (Eq, Show)

{-
data ArgEnum = ArgEnum
  { enumName :: String
  , enumEntries :: [Entry]
  , enumBitfield :: Maybe Bool
  , enumDescription :: Maybe Description
  } deriving (Eq, Show)

data Entry = Entry
  { entryName :: String
  , entryValue :: Integer
  , entrySummary :: Maybe String
  } deriving (Eq, Show)

data Argument = Argument
  { argumentName :: String
  , argumentType :: ArgProtDataBox
  , argumentNullable :: Bool
  , argumentSummary :: Maybe String
  } deriving (Eq, Show)
-}
