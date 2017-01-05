{-|
Module      : Graphics.Sudbury.Lifetime
Description : Object lifetime tracker
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

We need to track which side of the connection believes which objects
to exist.

In this module, we use the term "local" and "foreign" to distinguish
client and server without specifying which is which.
-}
{-# LANGUAGE LambdaCase #-}
module Graphics.Sudbury.Lifetime
  ( ObjectMap
  , Id
  , Lifetime

  , initLifetimeIO
  , allocId
  , foreignCreate
  , foreignDestroy
  , localCreate
  , localDestroy
  , withForeignObject
  , withLocalObject
  , withLocalObjectIO
  , withMutualObject
  ) where

import Data.Word
import qualified Data.IntMap as IM
import Control.Concurrent.STM

type ObjectMap a = IM.IntMap a

type Id = Word32

data Lifetime a = Lifetime
  { lifetimeLocal   :: TVar (ObjectMap a)
    -- ^ Objects that we believe exist
  , lifetimeForeign :: TVar (ObjectMap a)
    -- ^ Objects that the other side of the connection believes exist
  , lifetimeLastId :: TVar Id
    -- ^ highest allocated Id. -- TODO do we really need this?
  , lifetimeFreeIds :: TVar [Id]
    -- ^ recently deallocated Ids
  }

initLifetimeIO :: IO (Lifetime a)
initLifetimeIO = do
  last_id <- newTVarIO 1
  id_stack <- newTVarIO []
  object_map_local <- newTVarIO IM.empty
  object_map_foreign <- newTVarIO IM.empty
  return $ Lifetime object_map_local object_map_foreign last_id id_stack

-- | Obtain an appropriate Id for local use.
allocId :: Lifetime a -> STM Id
allocId l = do
  let avail  = lifetimeFreeIds l
      lastId = lifetimeLastId l
  free <- readTVar avail
  case free of
    (i:is) -> do
      writeTVar avail is
      return i
    [] -> do
      n <- readTVar lastId
      writeTVar lastId (n+1)
      return (n+1)

-- TODO what is the exact purpose of this?
deallocId :: Lifetime a -> Id -> STM ()
deallocId l i = modifyTVar (lifetimeFreeIds l) (i:)

-- | Put a new foreign object in the lifetime
foreignCreate :: Lifetime a -> Id -> a -> STM ()
foreignCreate l i o = do
  modifyTVar (lifetimeForeign l) (IM.insert (fromIntegral i) o)
  modifyTVar (lifetimeLocal   l) (IM.insert (fromIntegral i) o)

-- | Put a new local object in the lifetime
localCreate :: Lifetime a -> Id -> a -> STM ()
localCreate = foreignCreate


-- | The foreign side destroyed an object
foreignDestroy :: Lifetime a -> Id -> STM ()
foreignDestroy l i =
  IM.member (fromIntegral i) <$> readTVar (lifetimeForeign l) >>= \case
    False -> return () -- Id was already destroyed
    True  -> do
      modifyTVar (lifetimeForeign l) (IM.delete (fromIntegral i))
      -- If the Id was also destroyed locally, we can start using it again
      IM.member (fromIntegral i) <$> readTVar (lifetimeLocal l) >>= \case
        False -> deallocId l i
        True  -> return () -- still alive locally; don't free Id

-- | The foreign side destroyed an object
localDestroy :: Lifetime a -> Id -> STM ()
localDestroy l i =
  IM.member (fromIntegral i) <$> readTVar (lifetimeLocal l) >>= \case
    False -> return () -- Id was already destroyed
    True  -> do
      modifyTVar (lifetimeLocal l) (IM.delete (fromIntegral i))
      -- If the Id was also destroyed foreignly, we can start using it again
      IM.member (fromIntegral i) <$> readTVar (lifetimeForeign l) >>= \case
        False -> deallocId l i
        True  -> return () -- still alive foreignly; don't free Id

-- | Do something with an object that is the foreign party believes to
-- be alive, given its Id
withForeignObject :: Lifetime a -> Id -> (a -> STM b) -> STM (Maybe b)
withForeignObject l i f = do
  objectMap <- readTVar $ lifetimeForeign l
  case IM.lookup (fromIntegral i) objectMap of
    Nothing -> return Nothing
    Just o  -> do
      res <- f o
      return $ Just res

-- | Do something with an object that is locally alive, given its Id
withLocalObject :: Lifetime a -> Id -> (a -> STM b) -> STM (Maybe b)
withLocalObject l i f = do
  objectMap <- readTVar $ lifetimeLocal l
  case IM.lookup (fromIntegral i) objectMap of
    Nothing -> return Nothing
    Just o  -> do
      res <- f o
      return $ Just res

-- TODO this is a hack; shouldn't need this
withLocalObjectIO :: Lifetime a -> Id -> (a -> IO b) -> IO (Maybe b)
withLocalObjectIO l i f = do
  objectMap <- readTVarIO $ lifetimeLocal l
  case IM.lookup (fromIntegral i) objectMap of
    Nothing -> return Nothing
    Just o  -> do
      res <- f o
      return $ Just res

-- TODO does this method make sense?
-- | Do something with an object that both parties agree is alive,
-- given its Id
withMutualObject :: Lifetime a -> Id -> (a -> STM b) -> STM (Maybe b)
withMutualObject l i f = do
  localMap <- readTVar $ lifetimeLocal l
  foreignMap <- readTVar $ lifetimeLocal l
  case IM.lookup (fromIntegral i) localMap of
    Nothing -> return Nothing
    Just o  ->
      case IM.lookup (fromIntegral i) foreignMap of
        Nothing -> return Nothing
        Just _  -> do
          res <- f o
          return $ Just res
