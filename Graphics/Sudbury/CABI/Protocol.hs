{-|
Module      : Graphics.Sudbury.CABI.Protocol
Description : Read protocol data from a foreign pointer to a libwayland `wl_interface`
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.CABI.Protocol where

import Control.Monad ((>=>), zipWithM)
import Data.Char (isNumber)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Set (Set, fromList)
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import qualified Graphics.Sudbury.Protocol.Runtime.Types as RT
import Graphics.Sudbury.Argument

import Graphics.Sudbury.CABI.Structs
import Graphics.Sudbury.CABI.Common

readProtocol :: Ptr WL_interface -> IO RT.Protocol
readProtocol ptr = do
  ifaces <- iterateInterfaces [ptr]
  return $ RT.Protocol ifaces

iterateInterfaces :: [Ptr WL_interface] -> IO [RT.Interface]
iterateInterfaces oldPtrs = do
   res <- mapM (peek >=> readInterface) oldPtrs
   let interfaces = map fst res
       newPtrs    = concat $ concat $ map snd res
       newPtrSet  = fromList newPtrs :: Set (Ptr WL_interface)
       oldPtrSet  = fromList oldPtrs :: Set (Ptr WL_interface)
   if newPtrSet == oldPtrSet
      then return interfaces
      else iterateInterfaces newPtrs

readInterface :: WL_interface -> IO (RT.Interface , [[Ptr WL_interface]])
readInterface iface = do
  name <- peekCString (ifaceName iface)
  requests <- mapM (peekElemOff (ifaceMethods iface)) [0 .. (fromIntegral $ ifaceMethodCount iface)]
  events <- mapM (peekElemOff (ifaceEvents iface)) [0 .. (fromIntegral $ ifaceEventCount iface)]
  (requestObjs , requestIfaces)  <- unzip <$> mapM readMessage requests
  (eventObjs   , eventIfaces)    <- unzip <$> mapM readMessage events
  return
    ( RT.Interface name (fromIntegral $ ifaceVersion iface) requestObjs eventObjs []
    , requestIfaces ++ eventIfaces
    )
  -- enum data not provided by C ABI. TODO explain why we can pretend there are none

readMessage :: WL_message -> IO (RT.Message , [Ptr WL_interface])
readMessage msg = do
  name <- peekCString (msgName msg)
  signature <- peekCString (msgSignature msg)
  let (since , args) = readSignature signature
      areNullable = fst $ unzip args
      argTypes = snd $ unzip args
  argumentData <- zipWithM (goArg (msgInterfaces msg)) [0..] argTypes
  let arguments =
        zipWith
          -- name not provided TODO why can we pretend this is empty?
          (\argtype isNullable -> RT.Argument "" argtype isNullable Nothing)
          argumentData
          areNullable
  subIfaces <- peekArray (length arguments) (msgInterfaces msg)
  return
    ( RT.Message name arguments False (fromMaybe 0 since)
      -- destructor data not provided. TODO why can we pretend this is false?
    , subIfaces
    )
  where
    readInterfaceName :: Ptr (Ptr WL_interface) -> Int -> IO (Maybe String)
    readInterfaceName ptr i = do
      ifacePtr <- peekElemOff ptr i
      if ifacePtr == nullPtr
         then return Nothing
         else do
           iface <- peek ifacePtr
           Just <$> peekCString (ifaceName iface)
    readArgument :: Ptr (Ptr WL_interface) -> Int -> SArgumentType t -> IO (RT.ArgProtData t)
    -- enum data not provided. TODO explain why we can pretend there is none
    readArgument _   _ SIntWAT    = return Nothing
    readArgument _   _ SUIntWAT   = return Nothing
    readArgument _   _ SFixedWAT  = return ()
    readArgument _   _ SStringWAT = return ()
    readArgument ptr i SObjectWAT = readInterfaceName ptr i
    readArgument ptr i SNewIdWAT  = readInterfaceName ptr i
    readArgument _   _ SArrayWAT  = return ()
    readArgument _   _ SFdWAT     = return ()
    goArg :: Ptr (Ptr WL_interface) -> Int -> ArgTypeBox -> IO RT.ArgProtDataBox
    goArg ptr i (ArgTypeBox tp) = RT.ArgProtDataBox tp <$> readArgument ptr i tp

readSignature :: String -> (Maybe Int, [(Bool , ArgTypeBox)])
readSignature str =
  let versionStr = filter isNumber str
      remainder  = filter (not . isNumber) str
      version    = maybeRead versionStr
  in (version , go remainder)
  where
    maybeRead :: Read a => String -> Maybe a
    maybeRead = fmap fst . listToMaybe . reads
    go :: String -> [(Bool , ArgTypeBox)]
    go ('?' : xs) = goNullable xs
    go (x : xs)   = (False , charToArgType x) : go xs
    go [] = []
    goNullable :: String -> [(Bool , ArgTypeBox) ]
    goNullable (x:xs) = (True , charToArgType x) : go xs
    goNullable [] = error "wl_message signature ended early!"
