{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Socket where

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.ByteString    as BS
import Network.Socket
import System.Posix.Types
import System.Posix.IO
import Control.Monad
import Data.List

import Graphics.Sudbury.Socket.Wayland

import Arbitrary ()

tests :: Test
tests = testGroup "Socket tests"
  [ testProperty "Socket send/receive identity" prop_socket_id
  ]

makeSomeSocks :: PropertyM IO [Socket]
makeSomeSocks = do
  numSocks <- pick $ choose (0 , 14) :: PropertyM IO Int -- maximum 28 fds over the domain socket - see MAX_FDS
  socks <- forM [0..numSocks] $ \_ -> do
    (a,b) <- run $ socketPair AF_UNIX Stream defaultProtocol
    return [a,b]
  return $ concat socks

-- | Test if packing and then parsing a 'WirePackage' yields the original package.
prop_socket_id :: BS.ByteString -> Property
prop_socket_id bytes = BS.length bytes > 0 ==> monadicIO $ do
  socks <- makeSomeSocks
  someSocks <-  pick $ sublistOf socks
  (ins , outs) <- run $ socketPair AF_UNIX Stream defaultProtocol
  let (infd, outfd) = (Fd $ fdSocket ins , Fd $ fdSocket outs)
      someFds = map (Fd . fdSocket) someSocks
  _ <- run $ sendToWayland infd bytes someFds
  (outBytes , outFds) <- run $ recvFromWayland outfd
  assert $ outBytes == bytes
  assert $ length someFds == length outFds
  run $ mapM_ closeFd outFds
  run $ mapM_ close (socks \\ someSocks)
