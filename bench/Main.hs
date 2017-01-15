{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Criterion.Main
import GHC.Generics
import Graphics.Sudbury.WirePackages
import qualified Test.QuickCheck as Q
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as BB
import Data.Store
import Data.Store.Core
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Attoparsec.ByteString as A


import Control.DeepSeq
import Debug.Trace
import Control.Monad

import Data.Fixed
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Binary as AB
import System.Endian (getSystemEndianness, Endianness(..))

data WL23_8
instance HasResolution WL23_8 where
  resolution _ = 256 -- 2^8
-- Duplicate from tests/Arbitrary.hs. Should think of a better
-- solution in due time
instance Q.Arbitrary B.ByteString where
  arbitrary = do
    someBytes <- Q.arbitrary
    return . toStrict . toLazyByteString . mconcat $ map BB.word8 someBytes

instance Q.Arbitrary WirePackage where
  arbitrary = do
    sender <- Q.arbitrary
    opcode <- Q.arbitrary
    payloadSize <- Q.elements ([20..200] :: [Int])
    payload <- B.pack <$> Q.vectorOf payloadSize (Q.arbitrary :: Q.Gen Word8)
    let size    = fromIntegral $ 8 + payloadSize
    return WirePackage
      { wirePackageSender  = sender
      , wirePackageSize    = size
      , wirePackageOpcode  = opcode
      , wirePackagePayload = payload
      }
newtype WirePackageBinary = WirePackageBinary
  { unWpBin :: B.ByteString
  } deriving (Show, Eq)


instance Q.Arbitrary WirePackageBinary where
  arbitrary = Q.sized $ \ n ->
    do k <- Q.choose (2, n)
       WirePackageBinary . B.concat <$> replicateM k genAlignedWp
    where
      genAlignedWp :: Q.Gen B.ByteString
      genAlignedWp =
        do wp <- Q.arbitrary :: Q.Gen WirePackage
           let padBytes = fromIntegral $ wirePackageSize wp `mod` 4
               pad = B.pack $ replicate padBytes (0 :: Word8)
           return $ encode wp `B.append` pad

deriving instance Generic WirePackageStream
instance NFData WirePackageStream

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

pkgStream' :: A.Parser [WirePackage]
pkgStream' = A.many' parseWirePackage


randomWirePackage :: IO [WirePackage]
randomWirePackage = Q.generate $ Q.vectorOf 10000 (Q.arbitrary :: Q.Gen WirePackage)

randomByteString :: IO [B.ByteString]
randomByteString = Q.generate $ fmap unWpBin <$> Q.vectorOf 10000 Q.arbitrary

decodeAttos :: B.ByteString -> [WirePackage]
decodeAttos = unsafeRight . A.parseOnly (A.many' parseWirePackage)

unsafeRight (Right a) = a
  
encodeBS :: WirePackage -> B.ByteString
encodeBS = toStrict . toLazyByteString . wirePack 

decodedWorking = do
  a <- randomWirePackage
  print $ map (decodeEx . encodeBS) a == a


fromRight (Right a) = a

deriving instance Generic WirePackage
instance NFData WirePackage

main = do
  rb <- randomByteString
  rw <- randomWirePackage
  rb `deepseq` return ()
  rw `deepseq` return ()
  let decodeEx' = decodeEx :: B.ByteString -> WirePackageStream
  defaultMain 
    [ bgroup "parse_random" 
      [ bench "id" $ nf (map id) rb
      , bench "storeable" $ nf (map decodeEx') rb 
      , bench "attoParsec1" $ nf (map decodeAttos) rb
      ]
    , bgroup "encod_random" 
      [ bench "id" $ nf (map id) rw
      , bench "storeable" $ nf (map encode) rw 
      , bench "BS.Builder" $ nf (map encodeBS) rw
      ]
    ]


-- | Fixed-precision number: 23 bits of integer space, 8 bits precision
type Fixed23_8 = Fixed WL23_8

anyWord16he :: A.Parser Word16
anyWord16he = case getSystemEndianness of
                LittleEndian -> AB.anyWord16le
                BigEndian    -> AB.anyWord16be

anyWord32he :: A.Parser Word32
anyWord32he = case getSystemEndianness of
                LittleEndian -> AB.anyWord32le
                BigEndian    -> AB.anyWord32be


-- Inspired by `anyWordN` from attoparsec-binary

packNumBytes :: (FiniteBits a, Num a) => B.ByteString -> a
packNumBytes = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

anyIntN :: (FiniteBits a, Num a) => (B.ByteString -> a) -> A.Parser a
anyIntN = anyIntN' 0
  where byteSize :: (FiniteBits a) => a -> Int
        byteSize = (`div` 8) . finiteBitSize
        anyIntN' :: (FiniteBits a) => a -> (B.ByteString -> a) -> A.Parser a
        anyIntN' d = flip fmap $ A.take $ byteSize d

anyInt16be :: A.Parser Int16
anyInt16be = anyIntN packNumBytes
anyInt16le :: A.Parser Int16
anyInt16le = anyIntN $ packNumBytes . B.reverse
anyInt32be :: A.Parser Int32
anyInt32be = anyIntN packNumBytes
anyInt32le :: A.Parser Int32
anyInt32le = anyIntN $ packNumBytes . B.reverse

-- End of attoparsec-binary inspiration

anyInt16he :: A.Parser Int16
anyInt16he = case getSystemEndianness of
                LittleEndian -> anyInt16le
                BigEndian    -> anyInt16be

anyInt32he :: A.Parser Int32
anyInt32he = case getSystemEndianness of
                LittleEndian -> anyInt32le
                BigEndian    -> anyInt32be

data ServerClient = Server | Client
