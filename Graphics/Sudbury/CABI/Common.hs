{-|
Module      : Graphics.Sudbury.CABI.Common
Description : C ABI
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

We attempt to implement the ABI of the libwayland C library.
This implements some common tools to implement this ABI.
-}
module Graphics.Sudbury.CABI.Common where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Fixed
import Data.Char
import Data.Word
import Data.Monoid
import Control.Concurrent.STM
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array (peekArray0)
import Foreign.Marshal.Utils (with)
import System.Posix.Types

import Graphics.Sudbury.WireMessages
import Graphics.Sudbury.WirePackages
import Graphics.Sudbury.Argument

import Graphics.Sudbury.CABI.Structs

-- | A specific implementation (ie for one opcode) is some abstract function
-- pointer. Since libffi doesn't care anyway, we'll make it something trivial.
type Implementation = FunPtr ()
-- | Listeners are structs of function pointers, so essentially (C-style) arrays
type Listener = Ptr Implementation
type UserData = Ptr ()
type DispatcherData = Ptr ()

{-
 * A dispatcher takes five arguments:  The first is the dispatcher-specific
 * implementation data associated with the target object.  The second is the
 * object on which the callback is being invoked (either wl_proxy or
 * wl_resource).  The third and fourth arguments are the opcode the wl_messsage
 * structure corresponding to the callback being emitted.  The final argument
 * is an array of arguments received from the other process via the wire
 * protocol.
typedef int (*wl_dispatcher_func_t)(const void *, void *, uint32_t,
				    const struct wl_message *,
				    union wl_argument *);
-}
-- the return value is ignored (even by libwayland)
-- | dispatcher function. we represent the second argument (wl_proxy or wl_resource)
-- by an untyped StablePtr which we can later cast to the right type. sorry.
type DispatcherFunc = FunPtr (DispatcherData -> StablePtr () -> CUInt -> Ptr WL_message -> Ptr WL_arg -> IO CInt)

type CDispatcher = (DispatcherFunc, DispatcherData)

type HaskDispatcher = Word16 -> [CArgBox] -> IO ()

charToArgType :: Char -> ArgTypeBox
charToArgType c =
  case c of
       'i' -> ArgTypeBox SIntWAT
       'u' -> ArgTypeBox SUIntWAT
       'f' -> ArgTypeBox SFixedWAT
       's' -> ArgTypeBox SStringWAT
       'o' -> ArgTypeBox SObjectWAT
       'n' -> ArgTypeBox SNewIdWAT
       'a' -> ArgTypeBox SArrayWAT
       'h' -> ArgTypeBox SFdWAT
       o   -> error ("Unexpected argument '" ++ [o] ++ "' encountered")

-- TODO The filter in the following is probably a hack. We should probably use more information of the signature.
signatureToTypes :: CString -> IO [ArgTypeBox]
signatureToTypes sig = map charToArgType . filter isAlpha . map castCCharToChar <$> peekArray0 0 sig

-- | See `convert_arguments_to_ffi` in connection.c in libwayland
type family CArgument (t :: ArgumentType) where
  CArgument 'IntWAT = CInt
  CArgument 'UIntWAT = CUInt
  CArgument 'FixedWAT = CInt
  CArgument 'StringWAT = CString
  CArgument 'ObjectWAT = Ptr ()
  CArgument 'NewIdWAT = Ptr () -- NOTE this should be CUInt server-side
  CArgument 'ArrayWAT = Ptr WL_array
  CArgument 'FdWAT = Fd

data CArgBox = forall t. CArgBox (SArgumentType t) (CArgument t)

data CMessage = CMessage
  { cMessageSender :: Word32
  , cMessageOpcode :: Word16
  , cMessageArguments :: [CArgBox]
  }

type MessageQueue = TQueue WireMessage
type FdQueue = TQueue Fd

handleWireCArg :: (Word32 -> Ptr ()) -> FdQueue -> (CArgBox -> IO a) ->  WireArgBox -> IO a
handleWireCArg _ _   f (WireArgBox SIntWAT n)    = f (CArgBox SIntWAT (fromIntegral n))
handleWireCArg _ _   f (WireArgBox SUIntWAT n)   = f (CArgBox SUIntWAT (fromIntegral n))
handleWireCArg _ _   f (WireArgBox SFixedWAT (MkFixed n)) = f (CArgBox SFixedWAT (fromIntegral n))
handleWireCArg _ _   f (WireArgBox SStringWAT s) = B.useAsCString s $ \cstr -> f (CArgBox SStringWAT cstr)
handleWireCArg _ _   f (WireArgBox SObjectWAT 0) = f (CArgBox SObjectWAT nullPtr)
handleWireCArg _ _   f (WireArgBox SNewIdWAT 0)  = f (CArgBox SNewIdWAT nullPtr)
handleWireCArg om _  f (WireArgBox SObjectWAT o) = f (CArgBox SObjectWAT (om o))
handleWireCArg om _  f (WireArgBox SNewIdWAT n)  = f (CArgBox SNewIdWAT (om n))
handleWireCArg _ _   f (WireArgBox SArrayWAT a)  = B.useAsCStringLen a $ \(cstr, len) -> with (WL_array (fromIntegral len) (fromIntegral len) cstr) $ \ptr -> f (CArgBox SArrayWAT ptr)
handleWireCArg _ queue f (WireArgBox SFdWAT _)  = do
  Fd fd <- atomically $ readTQueue queue
  f (CArgBox SFixedWAT fd)

takeFds :: FdQueue -> STM [Fd]
takeFds queue = do
  x <- tryReadTQueue queue
  case x of
    Nothing -> return []
    Just fd -> do
      fds <- takeFds queue
      return $ fd : fds

serializeQueue1 :: MessageQueue -> BB.Builder -> STM BB.Builder
serializeQueue1 queue bs = do
  msg <- tryReadTQueue queue
  case msg of
    Nothing -> return bs
    Just x -> serializeQueue1 queue (bs <> wirePack (messageToPackage x))


serializeQueue :: MessageQueue -> STM B.ByteString
serializeQueue queue = BL.toStrict . BB.toLazyByteString <$> serializeQueue1 queue mempty

withStablePtr :: StablePtr a -> (a -> IO b) -> IO b
withStablePtr = flip withStablePtr'

withStablePtr' :: (a -> IO b) -> StablePtr a  -> IO b
withStablePtr' f ptr = deRefStablePtr ptr >>= f
