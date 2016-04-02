{-|
Module      : Graphics.Sudbury.WirePackages
Description : Reads ByteStrings into packages containing individual messages
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE Safe #-}
module Graphics.Sudbury.WirePackages where

import Data.Word
import Data.Monoid ((<>))
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE
import qualified Data.Attoparsec.ByteString as A

import Graphics.Sudbury.Internal

data WirePackage = WirePackage
  { wirePackageSender  :: Word32
  , wirePackageSize    :: Word16
  , wirePackageOpcode  :: Word16
  , wirePackagePayload :: B.ByteString
  } deriving (Eq, Show)

-- | Construct a wayland wire wirePackage
wirePack :: WirePackage -> BB.Builder
wirePack msg = wirePackBuilder
  (wirePackageSender msg)
  (wirePackageSize msg)
  (wirePackageOpcode msg)
  (BB.byteString $ wirePackagePayload msg)

-- | Construct a wayland wire wirePackage with payload given as a 'Builder'.
wirePackBuilder :: Word32 -> Word16 -> Word16 -> BB.Builder -> BB.Builder
wirePackBuilder sender size opcode payload =
  BBE.word32Host sender
  <>
  -- FIXME make byte order portable here
  BBE.word16Host opcode
  <>
  BBE.word16Host size
  <>
  payload

parseWirePackage :: A.Parser WirePackage
parseWirePackage = do
  sender <- anyWord32he
  opcode <- anyWord16he
  size   <- anyWord16he
  payload <- A.take (fromIntegral size - 8)
  return WirePackage { wirePackageSender  = sender
                 , wirePackageSize    = size
                 , wirePackageOpcode  = opcode
                 , wirePackagePayload = payload
                 }

pkgStream :: A.Parser [WirePackage]
pkgStream = A.many' parseWirePackage
