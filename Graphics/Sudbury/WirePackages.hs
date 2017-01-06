{-|
Module      : Graphics.Sudbury.WirePackages
Description : Reads ByteStrings into packages containing individual messages
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.WirePackages where

import Data.Word
import Data.Store
import Data.Store.Core
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB


data WirePackage = WirePackage
  { wirePackageSender  :: Word32
  , wirePackageSize    :: Word16
  , wirePackageOpcode  :: Word16
  , wirePackagePayload :: B.ByteString
  } deriving (Eq, Show)

-- Storable that writes in the Wayland binary format.
-- Allows us to use the optimized machinery for Data.Store
-- to gain faster performing decoding/encoding.
instance Store WirePackage where
  size = VarSize $ fromIntegral . wirePackageSize
  poke p = do
    poke $ wirePackageSender p
    poke $ wirePackageOpcode p 
    poke $ wirePackageSize p
    let (sourceFp, sourceOffset, sourceLength) = B.toForeignPtr $ wirePackagePayload p
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
  peek = do
    sen <- peek
    op <- peek
    sz <- peek
    let payloadSize = fromIntegral sz - 8
    plraw <- peekToPlainForeignPtr "Data.ByteString.ByteString" payloadSize
    let pl = B.PS plraw 0 payloadSize
    return $ WirePackage 
      { wirePackageSender=sen
      , wirePackageSize=sz 
      , wirePackageOpcode=op 
      , wirePackagePayload=pl
      }
  {-# INLINE size #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}

wirePack :: WirePackage -> BB.Builder
wirePack = BB.byteString . encode
