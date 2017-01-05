{-|
Module      : Graphics.Sudbury.Protocol.Runtime.Types
Description : Protocol data without docs
Copyright   : (c) Auke Booij, 2016
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.Runtime.Types where

import Graphics.Sudbury.Argument

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

type Description = (String , String)

data Protocol = Protocol
  { protocolInterfaces :: [Interface]
  } deriving (Eq, Show)

data Interface = Interface
  { interfaceName :: String
  , interfaceVersion :: Int
  , interfaceRequests :: [Message]
  , interfaceEvents :: [Message]
  , interfaceEnums :: [ArgEnum]
  } deriving (Eq, Show)

data Message = Message
  { messageName :: String
  , messageArguments :: [Argument]
  , messageIsDestructor :: Bool
  , messageSince :: Int
  } deriving (Eq, Show)

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
