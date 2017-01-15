{-|
This module contains some orphan instances used in testing.

They are orphan instances as to not clutter the library code,
and not force any dependency on QuickCheck.

This module should only be used in the test code of this project.
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Arbitrary where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Control.Monad
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Test.QuickCheck
import qualified Data.Store as S
import Data.Word

import Graphics.Sudbury.WirePackages

instance Arbitrary B.ByteString where
  arbitrary = do
    someBytes <- arbitrary
    return . toStrict . toLazyByteString . mconcat $ map BB.word8 someBytes

instance Arbitrary WirePackage where
  arbitrary = do
    sender <- arbitrary
    opcode <- arbitrary
    payload <- arbitrary
    let size    = fromIntegral $ 8 + B.length payload
    return WirePackage
      { wirePackageSender  = sender
      , wirePackageSize    = size
      , wirePackageOpcode  = opcode
      , wirePackagePayload = payload
      }

instance Arbitrary WirePackageStream where
  arbitrary = WirePackageStream <$> arbitrary

-- | Newtype wrapper for the binary format of Wayland. Allows
-- us to construct arbitrary messages from n WirePackages with
-- the required padded bytes.
newtype WirePackageBinary = WirePackageBinary
  { unWpBin :: B.ByteString
  } deriving (Show, Eq)


instance Arbitrary WirePackageBinary where
  arbitrary = sized $ \ n ->
    do k <- choose (2, n)
       WirePackageBinary . B.concat <$> replicateM k genAlignedWp
    where
      genAlignedWp :: Gen B.ByteString
      genAlignedWp =
        do wp <- arbitrary :: Gen WirePackage
           let padBytes = fromIntegral $ wirePackageSize wp `mod` 4
               pad = B.pack $ replicate padBytes (0 :: Word8)
           return $ S.encode wp `B.append` pad

