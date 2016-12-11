module Main where

import Criterion.Main
import Graphics.Sudbury.WirePackages
import Test.QuickCheck
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB
import Data.Store
import Data.Store.Core
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Attoparsec.ByteString
import Control.DeepSeq
import Data.Word
import Debug.Trace

-- Duplicate from tests/Arbitrary.hs. Should think of a better
-- solution in due time
instance Arbitrary B.ByteString where
  arbitrary = do
    someBytes <- arbitrary
    return . toStrict . toLazyByteString . mconcat $ map BB.word8 someBytes

instance Arbitrary WirePackage where
  arbitrary = do
    sender <- arbitrary
    opcode <- arbitrary
    payloadSize <- elements ([20..200] :: [Int])
    payload <- B.pack <$> vectorOf payloadSize (arbitrary :: Gen Word8)
    let size    = fromIntegral $ 8 + payloadSize
    return WirePackage
      { wirePackageSender  = sender
      , wirePackageSize    = size
      , wirePackageOpcode  = opcode
      , wirePackagePayload = payload
      }

randomWirePackage :: IO [WirePackage]
randomWirePackage = generate $ vectorOf 10000 (arbitrary :: Gen WirePackage)

randomByteString :: IO [B.ByteString]
randomByteString = fmap (map encodeBS) randomWirePackage 

decodeAtto :: B.ByteString -> Either String WirePackage
decodeAtto = parseOnly parseWirePackage
  
encodeBS :: WirePackage -> B.ByteString
encodeBS = toStrict . toLazyByteString . wirePack 

decodedWorking = do
  a <- randomWirePackage
  print $ map (decodeEx . encodeBS) a == a

encodedWorking = do
  a <- randomByteString
  print $ map (encode . fromRight . decodeAtto) a == a

fromRight (Right a) = a

instance NFData WirePackage

toInt :: Integral a => a -> Int
toInt = fromInteger . toInteger
{-# INLINE toInt #-}

instance Store WirePackage where
  size = VarSize $ toInt . wirePackageSize
  poke (WirePackage sen size op pl) = do
    poke sen 
    poke op 
    poke size 
    let (sourceFp, sourceOffset, sourceLength) = B.toForeignPtr pl
    pokeFromForeignPtr sourceFp sourceOffset sourceLength
  peek = do
    sen <- peek
    op <- peek
    size <- peek
    let payloadSize = toInt size - 8
    pl <- peekToPlainForeignPtr "Data.ByteString.ByteString" payloadSize
    return $ WirePackage sen size op (B.PS pl 0 payloadSize)
  {-# INLINE size #-}
  {-# INLINE peek #-}
  {-# INLINE poke #-}
  
main = do
  rb <- randomByteString
  rw <- randomWirePackage
  rb `deepseq` return ()
  rw `deepseq` return ()
  let decodeEx' = decodeEx :: B.ByteString -> WirePackage
  defaultMain 
    [ bgroup "parse_random" 
      [ bench "id" $ nf (map id) rb
      , bench "storeable" $ nf (map decodeEx') rb 
      , bench "attoParsec1" $ nf (map decodeAtto) rb
      ]
    , bgroup "encod_random" 
      [ bench "id" $ nf (map id) rw
      , bench "storeable" $ nf (map encode) rw 
      , bench "BS.Builder" $ nf (map encodeBS) rw
      ]
    ]
