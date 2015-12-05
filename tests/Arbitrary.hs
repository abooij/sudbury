{-|
This module contains some orphan instances used in testing.

They are orphan instances as to not clutter the library code,
and not force any dependency on QuickCheck.

This module should only be used in the test code of this project.
-}
module Arbitrary where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Test.QuickCheck

import Graphics.Sudbury.WirePackages
import Graphics.Sudbury.WireMessages
import Graphics.Sudbury.ProtocolXML

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
