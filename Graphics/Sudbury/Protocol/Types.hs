{-|
Module      : Graphics.Sudbury.Protocol.Types
Description : This exposes most of the types the protocol parser outputs
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.Protocol.Types where

import Graphics.Sudbury.Argument

data ArgProtDataBox = forall t. ArgProtDataBox (SArgumentType t) (ArgProtData t)

argDataProj :: ArgProtDataBox -> ArgTypeBox
argDataProj (ArgProtDataBox tp _) = ArgTypeBox tp

type family ArgProtData (t :: ArgumentType) where
  ArgProtData 'IntWAT = Maybe WLEnum
  ArgProtData 'UIntWAT = Maybe WLEnum
  ArgProtData 'FixedWAT = ()
  ArgProtData 'StringWAT = ()
  ArgProtData 'ObjectWAT = Maybe WLInterface
  ArgProtData 'NewIdWAT = Maybe WLInterface
  ArgProtData 'ArrayWAT = ()
  ArgProtData 'FdWAT = ()

type WLDescription = (String , String)

data WLProtocol = WLProtocol
  { protocolName :: String
  , protocolDescription :: Maybe WLDescription
  , protocolInterfaces :: [WLInterface]
  , protocolCopyright :: Maybe String
  } deriving (Eq, Show)

data WLInterface = WLInterface
  { interfaceName :: String
  , interfaceDescription :: Maybe WLDescription
  , interfaceVersion :: Int
  , interfaceSince :: Int
  , interfaceRequests :: [WLMessage]
  , interfaceEvents :: [WLMessage]
  , interfaceEnums :: [WLEnum]
  }
-- The instances for `WLInterface` have to be written manually, to avoid loops
-- in our circular data structure.
-- This is because WLArgumentType refers back to WLInterface
instance Eq WLInterface where
  iface1 == iface2 =
    (interfaceName iface1 == interfaceName iface2) &&
    (interfaceVersion iface1 == interfaceVersion iface2)
instance Show WLInterface where
  show iface =
    "WLInterface " ++
    interfaceName iface ++
    " (version " ++
    show (interfaceVersion iface) ++
    ")"

data WLMessage = WLMessage
  { messageName :: String
  , messageArguments :: [WLArgument]
  , messageIsDestructor :: Bool
  , messageSince :: Int
  , messageDescription :: Maybe WLDescription
  }

data WLEnum = WLEnum
  { enumName :: String
  , enumEntries :: [WLEntry]
  , enumBitfield :: Maybe Bool
  , enumDescription :: Maybe WLDescription
  }

data WLEntry = WLEntry
  { entryName :: String
  , entryValue :: Integer
  , entrySummary :: Maybe String
  }

data WLArgument = WLArgument
  { argumentName :: String
  , argumentType :: ArgProtDataBox
  , argumentNullable :: Bool
  , argumentSummary :: Maybe String
  }
