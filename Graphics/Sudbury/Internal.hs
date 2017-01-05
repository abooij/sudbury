{-|
Module      : Graphics.Sudbury.Internal
Description : Internal utilities for parsing ByteStrings into words and ints of
              various sizes
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX

The parsers in "Data.Attoparsec.Binary" are not quite sufficient for our use
case: we need to parse words in host endianness, and parse ints.
-}
module Graphics.Sudbury.Internal where

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
