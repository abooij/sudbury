{-|
Module      : Graphics.Sudbury.WireMessages
Description : Representation of messages parsed from the wayland wire protocol
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
{-# LANGUAGE Safe #-}
module Graphics.Sudbury.WireMessages where

import Data.Fixed
import Data.Word
import Data.Int
import Data.Monoid
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE

import Graphics.Sudbury.Internal
import Graphics.Sudbury.Argument
import Graphics.Sudbury.Protocol.Types

data WL23_8
instance HasResolution WL23_8 where
  resolution _ = 256 -- 2^8
-- | Fixed-precision number: 23 bits of integer space, 8 bits precision
type Fixed23_8 = Fixed WL23_8

type family UnboxedWireArgument (t :: ArgumentType) where
  UnboxedWireArgument 'IntWAT = Int32
  UnboxedWireArgument 'UIntWAT = Word32
  UnboxedWireArgument 'FixedWAT = Fixed23_8
  UnboxedWireArgument 'StringWAT = B.ByteString
  UnboxedWireArgument 'ObjectWAT = Word32
  UnboxedWireArgument 'NewIdWAT = Word32
  UnboxedWireArgument 'ArrayWAT = B.ByteString
  UnboxedWireArgument 'FdWAT = ()

data WireArgBox = forall t. WireArgBox (SArgumentType t) (UnboxedWireArgument t)

data WireMessage = WireMessage
  { wireMessageSender :: Word32
  , wireMessageMessage :: WLMessage
  , wireMessageArguments :: [WireArgBox]
  }

parseWireByteString :: A.Parser B.ByteString
parseWireByteString = do
  len' <- anyWord32he
  let len = fromIntegral len'
      size = 4 * (1 + ((len - 1) `div` 4))
      padding = size - len
  bs <- A.take len
  _  <- A.take padding
  return bs

-- | Decode an individual argument
parseWireArgument :: SArgumentType t -> A.Parser (UnboxedWireArgument t)
parseWireArgument SIntWAT = anyInt32he
parseWireArgument SUIntWAT = anyWord32he
parseWireArgument SFixedWAT = MkFixed . fromIntegral <$> anyInt32he
parseWireArgument SStringWAT = parseWireByteString
parseWireArgument SObjectWAT = anyWord32he
parseWireArgument SNewIdWAT = anyWord32he
parseWireArgument SArrayWAT= parseWireByteString
parseWireArgument SFdWAT = return ()

boxedParse :: ArgTypeBox -> A.Parser WireArgBox
boxedParse (ArgTypeBox tp) =
  WireArgBox tp <$> parseWireArgument tp

decode :: Word32 -> WLMessage -> A.Parser WireMessage
decode sender msg = do
  args <- mapM (boxedParse . argDataProj . argumentType) (messageArguments msg)
  return WireMessage
    { wireMessageSender = sender
    , wireMessageMessage = msg
    , wireMessageArguments = args
    }

-- | Encode some argument values into a ByteString.
--
--   This does not produce a full wayland package. See 'Graphics.Sudbury.WirePackages.packBuilder'.
encode :: [WireArgBox] -> BB.Builder
encode = mconcat . map boxedEncode

boxedEncode :: WireArgBox -> BB.Builder
boxedEncode (WireArgBox tp arg) = encodeArgument tp arg

buildWireByteString :: B.ByteString -> BB.Builder
buildWireByteString bs =
  BBE.word32Host (fromIntegral len)
  <>
  BB.byteString bs
  <>
  mconcat (replicate padding (BB.int8 0))
  where
    len = B.length bs
    size = 4 * (1 + ((len -1) `div` 4))
    padding = size - len

-- | Encode an individual argument
encodeArgument :: SArgumentType t -> UnboxedWireArgument t -> BB.Builder
encodeArgument SIntWAT i = BBE.int32Host i
encodeArgument SUIntWAT i = BBE.word32Host i
encodeArgument SFixedWAT (MkFixed n) = BBE.int32Host $ fromIntegral n
encodeArgument SStringWAT bytes = buildWireByteString bytes
encodeArgument SObjectWAT i = BBE.word32Host i
encodeArgument SNewIdWAT i = BBE.word32Host i
encodeArgument SArrayWAT bytes = buildWireByteString bytes
encodeArgument SFdWAT () = mempty
