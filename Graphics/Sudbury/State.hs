{-|
Module      : Graphics.Sudbury.State
Description : Wayland state tracker
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

Wayland is a stateful protocol. This code can track that state.
-}
module Graphics.Sudbury.State where

import Data.Word
import qualified Data.Map as M -- FIXME: Should this be Strict? or an IntMap?
import Data.List

import Graphics.Sudbury.Internal
import Graphics.Sudbury.Protocol.Types
import Graphics.Sudbury.WirePackages

{-
Some fields in this file should probably be strict to avoid memory leaks.
-}

-- placeholders until I figure out what these should be
type MyObjectType = String
type MyMessageType = WLMessage
type ProtocolSet = WLProtocol

{-
state-related operations:

- insert new_id style object into map with version
(increase state)
< id and run-time type
+ read WireMessage and extract new_ids, insert into map
< typed message
- find WLMessage corresponding to incoming WirePackage
< wirepackage
< protocol set
> WLMessage?
  - find interface type corresponding to sender ID (table lookup)
  - find message corresponding to opcode ID (protocol lookup)
(read state)
+ interpret incoming wirepackage as typed message
< wirepackage
< protocol set
> typed message
- remove object from map on destruction
< id
  - zombie?
(decrease state)
+ read (typed) message and extract destruction, remove from map
< typed message
+ read package and interpret as message, extract destruction, remove from map
< wirepackage
-}

-- | (The state segment of) this is known as 'wl_client' (compositor-side) or 'wl_display' (client-side) in libwayland.
data ConnectionState = ConnectionState
  { connectionObjects :: M.Map Word32 MyObjectType
  , connectionLastObjectId :: Word32
  }

-- What will our analogue to 'wl_global_bind_func_t' be?
data Global = Global
  { globalInterface :: WLInterface
  , globalType :: MyObjectType
  }

-- | This is known as 'wl_display' (compositor-side) in libwayland.
-- There is no client-side analogue.
-- The protocol is used to exchange this information from the compositor to the client.
data CompositorState = CompositorState
  { globalList :: M.Map Word32 Global
  }

-- What do we want to say about the display object?
initialConnectionState :: ConnectionState
initialConnectionState = ConnectionState M.empty 1

-- This should probably be populated
initialCompositorState :: CompositorState
initialCompositorState = CompositorState M.empty

-- todo: do something more interesting if the key already exists
insertNewId :: Word32 -> MyObjectType -> ConnectionState -> ConnectionState
insertNewId i tp s = s { connectionObjects = M.insert i tp (connectionObjects s) }

packageFindMessage :: ServerClient -> ProtocolSet -> WirePackage -> ConnectionState -> Maybe MyMessageType
packageFindMessage sc ps pack s = do
  tp <- M.lookup (wirePackageSender pack) (connectionObjects s)
  iface <- find (\i -> interfaceName i == tp) (protocolInterfaces ps)
  -- FIXME the following can fail!
  let msg = case sc of
                Server -> interfaceRequests iface !! fromIntegral (wirePackageOpcode pack)
                Client -> interfaceEvents iface !! fromIntegral (wirePackageOpcode pack)
  return msg

removeObject :: Word32 -> ConnectionState -> ConnectionState
removeObject i s = s { connectionObjects = M.delete i (connectionObjects s) }
