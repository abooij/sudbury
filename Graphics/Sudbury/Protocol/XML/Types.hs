{-|
Module      : Graphics.Sudbury.Protocol.XML.Types
Description : Defines types for an XML definition of a wayland protocol
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.XML.Types where

import Graphics.Sudbury.Argument

type Description = (String , String)

data ArgProtDataBox et it = forall t. ArgProtDataBox (SArgumentType t) (ArgProtData et it t)

argDataProj :: ArgProtDataBox et it -> ArgTypeBox
argDataProj (ArgProtDataBox tp _) = ArgTypeBox tp

instance Eq (ArgProtDataBox et it) where
  x == y = argDataProj x == argDataProj y
instance Show (ArgProtDataBox et it) where
  show x = "ArgProtDataBox " ++ show (argDataProj x) ++ " [...]"

type family ArgProtData (et :: *) (it :: *) (t :: ArgumentType) where
  ArgProtData et _  'IntWAT = Maybe et
  ArgProtData et _  'UIntWAT = Maybe et
  ArgProtData _  _  'FixedWAT = ()
  ArgProtData _  _  'StringWAT = ()
  ArgProtData _  it 'ObjectWAT = Maybe it
  ArgProtData _  it 'NewIdWAT = Maybe it
  ArgProtData _  _  'ArrayWAT = ()
  ArgProtData _  _  'FdWAT = ()

data Protocol et it = Protocol
  { protocolName :: Maybe String
  , protocolDescription :: Maybe Description
  , protocolInterfaces :: [Interface et it]
  , protocolCopyright :: Maybe String
  } deriving (Eq, Show)

data Interface et it = Interface
  { interfaceName :: String
  , interfaceDescription :: Maybe Description
  , interfaceVersion :: Int
  , interfaceRequests :: [Message et it]
  , interfaceEvents :: [Message et it]
  , interfaceEnums :: [ArgEnum]
  } deriving (Eq, Show)

data Message et it = Message
  { messageName :: String
  , messageArguments :: [Argument et it]
  , messageIsDestructor :: Bool
  , messageSince :: Maybe Int
  , messageDescription :: Maybe Description
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

data Argument et it = Argument
  { argumentName :: String
  , argumentType :: ArgProtDataBox et it
  , argumentNullable :: Bool
  , argumentSummary :: Maybe String
  } deriving (Eq, Show)

type XMLArgument = Argument String String
type XMLMessage = Message String String
type XMLInterface = Interface String String
type XMLProtocol = Protocol String String
