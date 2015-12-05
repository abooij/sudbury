{-|
Module      : Graphics.Sudbury.WireMessages
Description : Representation of messages parsed from the wayland wire protocol
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
-}
module Graphics.Sudbury.WireMessages where

import Data.Fixed
import Data.Word
import Data.Int
import Data.Monoid
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BBE

import Graphics.Sudbury.ProtocolXML as P
import Graphics.Sudbury.Internal

data WL23_8
instance HasResolution WL23_8 where
  resolution _ = 256 -- 2^8
-- | Fixed-precision number: 23 bits of integer space, 8 bits precision
type Fixed23_8 = Fixed WL23_8

data WireArgument = IntArg Int32
                  | UIntArg Word32
                  | FixedArg Fixed23_8
                  | StringArg B.ByteString
                  | ObjectArg Word32
                  | NewIdArg Word32
                  | ArrayArg B.ByteString
                  | FDArg

data WireMessage = WireMessage
  { wireMessageSender :: Word32
  , wireMessageMessage :: WLMessage
  , wireMessageArguments :: [WireArgument]
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
-- Comments for each type are from the wayland documentation
parseWireArgument :: WLArgumentType -> A.Parser WireArgument
-- The value is the 32-bit value of the signed/unsigned int.
parseWireArgument (WLInt _) = IntArg <$> anyInt32he
parseWireArgument (WLUInt _) = UIntArg <$> anyWord32he
-- Signed 24.8 decimal numbers. It is a signed decimal type which offers a sign
-- bit, 23 bits of integer precision and 8 bits of decimal precision. This is
-- exposed as an opaque struct with conversion helpers to and from double and
-- int on the C API side.
parseWireArgument WLFixed = FixedArg . MkFixed . fromIntegral <$> anyInt32he
-- Starts with an unsigned 32-bit length, followed by the string contents,
-- including terminating null byte, then padding to a 32-bit boundary.
parseWireArgument WLString = StringArg <$> parseWireByteString
-- 32-bit object ID.
parseWireArgument (WLObject _) = ObjectArg <$> anyWord32he
-- The 32-bit object ID. On requests, the client decides the ID. The only events
-- with new_id are advertisements of globals, and the server will use IDs below
-- 0x10000.
parseWireArgument (WLNewId _) = NewIdArg <$> anyWord32he
-- Starts with 32-bit array size in bytes, followed by the array contents
-- verbatim, and finally padding to a 32-bit boundary.
parseWireArgument WLArray = ArrayArg <$> parseWireByteString
-- The file descriptor is not stored in the message buffer, but in the ancillary
-- data of the UNIX domain socket message (msg_control).
parseWireArgument WLFD = return FDArg

decode :: Word32 -> WLMessage -> A.Parser WireMessage
decode sender msg = do
  args <- mapM (parseWireArgument . argumentType) (messageArguments msg)
  return WireMessage
    { wireMessageSender = sender
    , wireMessageMessage = msg
    , wireMessageArguments = args
    }

-- | Encode some argument values into a ByteString.
--
--   This does not produce a full wayland package. See 'Graphics.Sudbury.WirePackages.packBuilder'.
encode :: [WireArgument] -> BB.Builder
encode = mconcat . map encodeArgument

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
encodeArgument :: WireArgument -> BB.Builder
-- Comments for each type are from the wayland documentation
-- The value is the 32-bit value of the signed/unsigned int.
encodeArgument (IntArg i) = BBE.int32Host i
encodeArgument (UIntArg i) = BBE.word32Host i
-- Signed 24.8 decimal numbers. It is a signed decimal type which offers a sign
-- bit, 23 bits of integer precision and 8 bits of decimal precision. This is
-- exposed as an opaque struct with conversion helpers to and from double and
-- int on the C API side.
encodeArgument (FixedArg (MkFixed n)) = BBE.int32Host $ fromIntegral n
-- Starts with an unsigned 32-bit length, followed by the string contents,
-- including terminating null byte, then padding to a 32-bit boundary.
encodeArgument (StringArg str) = buildWireByteString str
-- 32-bit object ID.
encodeArgument (ObjectArg o) = BBE.word32Host o
-- The 32-bit object ID. On requests, the client decides the ID. The only events
-- with new_id are advertisements of globals, and the server will use IDs below
-- 0x10000.
encodeArgument (NewIdArg o) = BBE.word32Host o
-- Starts with 32-bit array size in bytes, followed by the array contents
-- verbatim, and finally padding to a 32-bit boundary.
encodeArgument (ArrayArg a) = buildWireByteString a
-- The file descriptor is not stored in the message buffer, but in the ancillary
-- data of the UNIX domain socket message (msg_control).
encodeArgument FDArg = mempty
