{-|
Module      : Graphics.Sudbury.WirePackages
Description : Reads ByteStrings into packages containing individual messages
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.WirePackages where

import Data.Word
import Data.Monoid 
import Data.Store
import Data.Foldable
import Data.Store.Core
import Data.Store.Internal
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad
import Foreign.Ptr (minusPtr, plusPtr, Ptr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)

import Control.Exception (Exception(..), throwIO, try)




data WirePackage = WirePackage
  { wirePackageSender  :: Word32
  , wirePackageSize    :: Word16
  , wirePackageOpcode  :: Word16
  , wirePackagePayload :: B.ByteString
  } deriving (Eq, Show)

-- | Storable that writes in the Wayland binary format.
-- Allows us to use the optimized machinery from Data.Store
instance Store WirePackage where
  size = VarSize $ \x -> let sz = fromIntegral (wirePackageSize x) in sz + (sz `mod` 4)
  poke p = do
    poke $ wirePackageSender p
    poke $ wirePackageOpcode p 
    poke $ wirePackageSize p
    let (sourceFp, sourceOffset, sourceLength) = B.toForeignPtr $ wirePackagePayload p
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
    pokeAlign p
    where
      {-# INLINE pokeAlign #-}
      pokeAlign wp = let align = fromIntegral $ wirePackageSize wp `mod` 4
                         emptyByte = 0 :: Word8 -- Maybe skipPoke would be better
                      in replicateM_ align (poke emptyByte)
  peek = do
    sen <- peek
    op <- peek
    sz <- peek
    let payloadSize = fromIntegral sz - 8
    plraw <- peekToPlainForeignPtr "Data.ByteString.ByteString" payloadSize
    let pl = B.PS plraw 0 payloadSize
    skip $ fromIntegral sz `mod` 4
    return $ WirePackage 
      { wirePackageSender  = sen
      , wirePackageSize    = sz
      , wirePackageOpcode  = op
      , wirePackagePayload = pl
      }
  {-# INLINE size #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}

-- | Newtype wrapper around a list of WirePackages is 
-- necessary to write the correct amount of pad bytes
-- and to account for non-prepending of list length
newtype WirePackageStream = WirePackageStream 
  { unWirePackageStream :: [WirePackage]
  } deriving (Eq, Show)

-- | Storable instance for a number of Wayland wirepackages
-- that accounts for padding and deserializing.
instance Store WirePackageStream where
  size = VarSize $ getSum . foldMap (Sum . f) . unWirePackageStream
    where
      VarSize f = size :: Size WirePackage
  
  poke = traverse_ poke . unWirePackageStream

  peek = undefined
  {-# INLINE size #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}

decodeMany :: Store a => B.ByteString -> Either PeekException [a]
decodeMany = unsafePerformIO . try . decodeIOMany

decodeIOMany :: Store a => B.ByteString -> IO [a]
decodeIOMany = decodeIOWithMany peek

decodeIOWithMany :: Peek a -> B.ByteString -> IO [a]
decodeIOWithMany mypeek (B.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOWithFromPtrMany mypeek ptr len
{-# INLINE decodeIOWithMany #-}

decodeIOWithFromPtrMany :: Peek a -> Ptr Word8 -> Int -> IO [a]
decodeIOWithFromPtrMany mypeek ptr len = do
  if len < 8
     then return []
     else do
         (offset, x) <- decodeIOPortionWithFromPtr mypeek ptr len
         (x : ) <$> decodeIOWithFromPtrMany mypeek (ptr `plusPtr` offset) (len - offset)
       --throwIO $ PeekException (len - offset) "Didn't consume all input."
{-# INLINE decodeIOWithFromPtrMany #-}

wirePack :: WirePackage -> BB.Builder
wirePack = BB.byteString . encode


