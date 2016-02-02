{-|
Module      : Graphics.Sudbury.Crap.Client
Description : C API crap
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

The wayland C API has some free crap included in it.
This implements client crap.

The code in this module could benefit from some optics...
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Sudbury.Crap.Client where

import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Data.Maybe (fromMaybe, catMaybes)
import Control.Concurrent.STM
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Array (peekArray0)
import System.Posix.Types (Fd(..))
import Network.Socket (fdSocket)

import Graphics.Sudbury.Argument
import Graphics.Sudbury.Socket
import Graphics.Sudbury.WireMessages

import Graphics.Sudbury.Crap.Common
import Graphics.Sudbury.Crap.Structs

-- stubs
data TODO
type MyEvent = TODO

type EventQueue = TQueue MyEvent
type MessageQueue = TQueue WireMessage
type FdQueue = TQueue Fd

data Proxy = Proxy
  { proxyParent :: Maybe Proxy
  , proxyDisplayData :: DisplayData
  , proxyDisplay :: Proxy
  , proxyQueue :: TVar (StablePtr EventQueue)
  , proxyListener :: TVar (Maybe (Either Listener Dispatcher))  -- ^ listener/implementation or dispatcher (with data)
  , proxyUserData :: TVar UserData
  , proxyId :: Word32
  , proxyInterface :: Ptr WL_interface
  -- flags: destroyed, id_deleted?
  --
  }

data DisplayData = DisplayData
  { displayFd :: Fd
  , displayErr :: TVar (Maybe (Int32, Maybe (Word32 , Ptr WL_interface , Word32)))
    -- ^ last_error, code, interface, id
  , displayDefaultQueue :: StablePtr EventQueue -- display_queue should be the proxy's queue
  , displayOutQueue :: MessageQueue -- messages to write to the fd
  , displayFdQueue :: FdQueue -- fd's to be copied to the Other Side (tm)
  , displayLastId :: TVar Word32
  , displayFreeIds :: TVar [Word32]
  -- mutex?
  -- reader_cond?
  -- read_serial?
  -- reader_count?
  }

type family CArgument (t :: ArgumentType) where
  CArgument 'IntWAT = CInt
  CArgument 'UIntWAT = CUInt
  CArgument 'FixedWAT = CInt
  CArgument 'StringWAT = CString
  CArgument 'ObjectWAT = StablePtr Proxy
  CArgument 'NewIdWAT = CUInt
  CArgument 'ArrayWAT = Ptr WL_array
  CArgument 'FdWAT = Fd

data CArgBox = forall t. CArgBox (SArgumentType t) (CArgument t)

-- we basically only need to pattern match on the first argument here because the Storable instances don't propagate along type families
readCArg :: Ptr WL_arg -> SArgumentType t -> IO (CArgument t)
readCArg p SIntWAT    = peek (castPtr p)
readCArg p SUIntWAT   = peek (castPtr p)
readCArg p SFixedWAT  = peek (castPtr p)
readCArg p SStringWAT = peek (castPtr p)
readCArg p SObjectWAT = peek (castPtr p)
readCArg p SNewIdWAT  = peek (castPtr p)
readCArg p SArrayWAT  = peek (castPtr p)
readCArg p SFdWAT     = peek (castPtr p)

cArgToWireArg :: SArgumentType t -> CArgument t -> IO (WireArgument t , Maybe Fd)
cArgToWireArg SIntWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SUIntWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SFixedWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SStringWAT cstr = do
  bs <- BS.packCString cstr
  return (bs , Nothing)
cArgToWireArg SObjectWAT proxy = do
  proxyVal <- deRefStablePtr proxy
  return (proxyId proxyVal , Nothing)
cArgToWireArg SNewIdWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SArrayWAT ar = do
  array <- peek ar
  bs <- BS.packCStringLen (arrayData array , fromIntegral $ arraySize array)
  return (bs , Nothing)
cArgToWireArg SFdWAT fd = do
  error "fd undefined" -- TODO
  -- here we should duplicate the Fd and return it in the second argument

getWireArg :: Ptr WL_arg -> SArgumentType t -> IO (WireArgument t , Maybe Fd)
getWireArg ptr x = do
  cArg <- readCArg ptr x
  cArgToWireArg x cArg

{-

struct wl_proxy;

struct wl_display;

/** \class wl_event_queue
 *
 * \brief A queue for \ref wl_proxy object events.
 *
 * Event queues allows the events on a display to be handled in a thread-safe
 * manner. See \ref wl_display for details.
 *
 */
struct wl_event_queue;
-}

-- No need to reify these types

{-
void
wl_event_queue_destroy(struct wl_event_queue *queue);
-}

foreign export ccall "wl_event_queue_destroy" freeStablePtr
  :: StablePtr EventQueue -> IO ()

{-
-- cbits to _array
void
wl_proxy_marshal(struct wl_proxy *p, uint32_t opcode, ...);
-}

-- implemented in C to deal with vararg stuff natively

{-
void
wl_proxy_marshal_array(struct wl_proxy *p, uint32_t opcode,
		       union wl_argument *args);
-}

{-
struct wl_proxy *
wl_proxy_create(struct wl_proxy *factory,
		const struct wl_interface *interface);
-}

proxy_create :: StablePtr Proxy -> Ptr WL_interface -> IO (StablePtr Proxy)
proxy_create factory interface = do
  factVal <- deRefStablePtr factory
  queueVar <- atomically $ do
    factQueue <- readTVar (proxyQueue factVal)
    newTVar factQueue
  listenerVar <- newTVarIO Nothing
  dataVar <- newTVarIO nullPtr
  let dd = proxyDisplayData factVal
  pid <- atomically $ generateId (displayLastId dd) (displayFreeIds dd)

  newStablePtr $ Proxy
    { proxyParent = Just factVal
    , proxyDisplay = proxyDisplay factVal
    , proxyDisplayData = proxyDisplayData factVal
    , proxyQueue = queueVar
    , proxyListener = listenerVar
    , proxyUserData = dataVar
    , proxyId = pid
    , proxyInterface = interface
    }

{-
-- cbits to _array_constructor
struct wl_proxy *
wl_proxy_marshal_constructor(struct wl_proxy *proxy,
			     uint32_t opcode,
			     const struct wl_interface *interface,
			     ...);
-}

-- implemented in C to deal with vararg stuff natively

{-
struct wl_proxy *
wl_proxy_marshal_array_constructor(struct wl_proxy *proxy,
				   uint32_t opcode, union wl_argument *args,
				   const struct wl_interface *interface);
-}

proxy_msg_get_signature :: StablePtr Proxy -> CUInt -> IO (Ptr CChar)
proxy_msg_get_signature proxy opcode = do
  proxyVal <- deRefStablePtr proxy
  iface <- peek (proxyInterface proxyVal)
  msg <- peekElemOff (ifaceMethods iface) (fromIntegral opcode)
  return (msgSignature msg)

foreign export ccall "proxy_msg_get_signature" proxy_msg_get_signature
  :: StablePtr Proxy -> CUInt -> IO (Ptr CChar)

proxy_marshal_array_constructor :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> Ptr WL_interface -> IO (StablePtr Proxy)
proxy_marshal_array_constructor factory opcode args interface = do
  factVal <- deRefStablePtr factory
  proxy <- proxy_create factory interface -- TODO execute in STM rather than IO
  iface <- peek (proxyInterface factVal)
  msg <- peekElemOff (ifaceMethods iface) (fromIntegral opcode)
  argTypes <- map charToArgType <$> peekArray0 0 (msgSignature msg)
  -- TODO the following two expressions should be made prettier
  let ptrs = iterate (flip plusPtr wl_arg_size) args
  haskArgs <- sequence $ zipWith (
    \(ArgTypeBox x) ptr -> do
      (a , b) <- getWireArg ptr x
      return (WireArgBox x a , b)
    ) argTypes ptrs
  let wireMsg = WireMessage
       { wireMessageSender = (proxyId factVal)
       , wireMessageMessage = fromIntegral opcode
       , wireMessageArguments = fst $ unzip haskArgs
       }
  let fds = catMaybes $ snd $ unzip haskArgs
  let dd = proxyDisplayData factVal
      out_queue = displayOutQueue dd
      fd_queue  = displayFdQueue dd
  atomically $ do
    writeTQueue out_queue wireMsg
    mapM_ (writeTQueue fd_queue) fds
  -- TODO actually send data (as in libwayland)
  return proxy

foreign export ccall "wl_proxy_marshal_array_constructor" proxy_marshal_array_constructor
  :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> Ptr WL_interface -> IO (StablePtr Proxy)

{-
void
wl_proxy_destroy(struct wl_proxy *proxy);
-}

-- this function is dangerous for values of type Ptr Proxy that were not created using wl_proxy_create
-- (but this is nothing new w.r.t. the C side: don't destroy stuff you didn't create)
proxy_destroy :: StablePtr Proxy -> IO ()
proxy_destroy proxy = do
  -- TODO update object map (ie free proxy id)
  freeStablePtr proxy

foreign export ccall "wl_proxy_destroy" proxy_destroy
  :: StablePtr Proxy -> IO ()

{-
int
wl_proxy_add_listener(struct wl_proxy *proxy,
		      void (**implementation)(void), void *data);
-}

-- | Fill a TVar Maybe with a value if its current value is Nothing, and return the written value.
writeMaybeTVar :: TVar (Maybe a) -> a -> STM (Maybe a)
writeMaybeTVar var val = do
  modifyTVar var (Just . fromMaybe val)
  readTVar var

-- | Fill a TVar Maybe with a value if its current value is Nothing, and return if anything was written.
writeMaybeTVar' :: TVar (Maybe a) -> a -> STM Bool
writeMaybeTVar' var val = do
  current <- readTVar var
  case current of
    Nothing -> do
      writeTVar var (Just val)
      return True
    Just _  -> do
      return False


proxy_add_listener :: StablePtr Proxy -> Listener -> UserData -> IO CInt
proxy_add_listener proxy listener userdata = do
  proxyVal <- deRefStablePtr proxy
  atomically $ do
    written <- writeMaybeTVar' (proxyListener proxyVal) (Left listener)
    case written of
      True  -> do
        writeTVar (proxyUserData proxyVal) userdata
        return 0
      False -> do
        return (-1)

foreign export ccall "wl_proxy_add_listener" proxy_add_listener
  :: StablePtr Proxy -> Listener -> UserData -> IO CInt

{-
const void *
wl_proxy_get_listener(struct wl_proxy *proxy);
-}

proxy_get_listener :: StablePtr Proxy -> IO Listener
proxy_get_listener proxy = do
  proxyVal <- deRefStablePtr proxy
  val <- readTVarIO (proxyListener proxyVal)
  return $ case val of
    Just (Left x) -> x
    _             -> nullPtr

foreign export ccall "wl_proxy_get_listener" proxy_get_listener
  :: StablePtr Proxy -> IO Listener

{-
int
wl_proxy_add_dispatcher(struct wl_proxy *proxy,
			wl_dispatcher_func_t dispatcher_func,
			const void * dispatcher_data, void *data);
-}

proxy_add_dispatcher :: StablePtr Proxy -> DispatcherFunc -> DispatcherData -> UserData -> IO CInt
proxy_add_dispatcher proxy dispatcher ddata udata = do
  proxyVal <- deRefStablePtr proxy
  atomically $ do
    written <- writeMaybeTVar' (proxyListener proxyVal) (Right (dispatcher , ddata))
    case written of
      True  -> do
        writeTVar (proxyUserData proxyVal) udata
        return 0
      False -> do
        return (-1)

foreign export ccall "wl_proxy_add_dispatcher" proxy_add_dispatcher
  :: StablePtr Proxy -> DispatcherFunc -> DispatcherData -> UserData -> IO CInt

{-
void
wl_proxy_set_user_data(struct wl_proxy *proxy, void *user_data);
-}

proxy_set_user_data :: StablePtr Proxy -> UserData -> IO ()
proxy_set_user_data proxy udata = do
  proxyVal <- deRefStablePtr proxy
  atomically $ writeTVar (proxyUserData proxyVal) udata

foreign export ccall "wl_proxy_set_user_data" proxy_set_user_data
  :: StablePtr Proxy -> UserData -> IO ()

{-
void *
wl_proxy_get_user_data(struct wl_proxy *proxy);
-}

proxy_get_user_data :: StablePtr Proxy -> IO UserData
proxy_get_user_data proxy = do
  proxyVal <- deRefStablePtr proxy
  readTVarIO (proxyUserData proxyVal)

foreign export ccall "wl_proxy_get_user_data" proxy_get_user_data
  :: StablePtr Proxy -> IO UserData

{-
uint32_t
wl_proxy_get_id(struct wl_proxy *proxy);
-}

proxy_get_id :: StablePtr Proxy -> IO CUInt
proxy_get_id proxy = do
  proxyVal <- deRefStablePtr proxy
  return $ fromIntegral $ proxyId proxyVal

foreign export ccall "wl_proxy_get_id" proxy_get_id
  :: StablePtr Proxy -> IO CUInt

{-
const char *
wl_proxy_get_class(struct wl_proxy *proxy);
-}

-- TODO
-- think about resource management

{-
void
wl_proxy_set_queue(struct wl_proxy *proxy, struct wl_event_queue *queue);
-}

proxy_set_queue :: StablePtr Proxy -> StablePtr EventQueue -> IO ()
proxy_set_queue proxy queue = do
  proxyVal <- deRefStablePtr proxy
  atomically $ writeTVar (proxyQueue proxyVal) queue

foreign export ccall "wl_proxy_set_queue" proxy_set_queue
  :: StablePtr Proxy -> StablePtr EventQueue -> IO ()
{-
struct wl_display *
wl_display_connect(const char *name);
-}
peekCStringMaybe :: CString -> IO (Maybe String)
peekCStringMaybe cstr
  | cstr == nullPtr = return Nothing
  | otherwise       = Just <$> peekCString cstr

foreign import ccall unsafe "wayland-client.h &wl_display_interface" display_interface :: Ptr WL_interface

display_connect :: CString -> IO (StablePtr Proxy)
display_connect cstr = do
  str <- peekCStringMaybe cstr

  sock <- findServerSocketWithName' str >>= connectToServer
  let fd = Fd $ fdSocket sock
  err <- newTVarIO Nothing
  default_queue <- newTQueueIO >>= newStablePtr
  out_queue <- newTQueueIO
  fd_queue <- newTQueueIO
  last_id <- newTVarIO 0
  id_stack <- newTVarIO []

  let dd = DisplayData
        { displayFd = fd
        , displayErr = err
        , displayDefaultQueue = default_queue
        , displayOutQueue = out_queue
        , displayFdQueue = fd_queue
        , displayLastId = last_id
        , displayFreeIds = id_stack
        }

  queue <- newTQueueIO >>= newStablePtr >>= newTVarIO
  listener <- newTVarIO Nothing
  udata <- newTVarIO nullPtr
  let proxy = Proxy
        { proxyParent = Nothing
        , proxyDisplayData = dd
        , proxyDisplay = proxy
        , proxyQueue = queue
        , proxyListener = listener
        , proxyUserData = udata
        , proxyId = 0
        , proxyInterface = display_interface
        }
  newStablePtr proxy

foreign export ccall "wl_display_connect" display_connect
  :: CString -> IO (StablePtr Proxy)

{-
struct wl_display *
wl_display_connect_to_fd(int fd);
-}

-- TODO

{-
void
wl_display_disconnect(struct wl_display *display);
-}

-- TODO

{-
int
wl_display_get_fd(struct wl_display *display);
-}

display_get_fd :: StablePtr Proxy -> IO Fd
display_get_fd proxy = do
  proxyVal <- deRefStablePtr proxy
  return $ displayFd $ proxyDisplayData proxyVal

foreign export ccall "wl_display_get_fd" display_get_fd
  :: StablePtr Proxy -> IO Fd

{-
int
wl_display_dispatch(struct wl_display *display);
-}

display_dispatch :: StablePtr Proxy -> IO CInt
display_dispatch proxy = do
  proxyVal <- deRefStablePtr proxy
  default_queue <- deRefStablePtr $ displayDefaultQueue $ proxyDisplayData proxyVal
  display_dispatch_queue' proxyVal default_queue

foreign export ccall "wl_display_dispatch" display_dispatch
  :: StablePtr Proxy -> IO CInt

{-
int
wl_display_dispatch_queue(struct wl_display *display,
			  struct wl_event_queue *queue);
-}

display_dispatch_queue' :: Proxy -> EventQueue -> IO CInt
display_dispatch_queue' _ _ = error "dispatch undefined" -- TODO implement

{-
int
wl_display_dispatch_queue_pending(struct wl_display *display,
				  struct wl_event_queue *queue);
-}

display_dispatch_queue_pending' :: Proxy -> EventQueue -> IO CInt
display_dispatch_queue_pending' _ _ = error "dispatch pending undefined" -- TODO implement

{-
int
wl_display_dispatch_pending(struct wl_display *display);
-}

display_dispatch_pending :: StablePtr Proxy -> IO CInt
display_dispatch_pending proxy = do
  proxyVal <- deRefStablePtr proxy
  default_queue <- deRefStablePtr $ displayDefaultQueue $ proxyDisplayData proxyVal
  display_dispatch_queue_pending' proxyVal default_queue

foreign export ccall "wl_display_dispatch_pending" display_dispatch_pending
  :: StablePtr Proxy -> IO CInt

{-
int
wl_display_get_error(struct wl_display *display);
-}

display_get_error :: StablePtr Proxy -> IO CInt
display_get_error proxy = do
  proxyVal <- deRefStablePtr proxy
  atomically $ do
    err <- readTVar $ displayErr $ proxyDisplayData proxyVal
    case err of
      Nothing -> return 0
      Just ( last_error , _ ) -> return $ fromIntegral last_error

{-
uint32_t
wl_display_get_protocol_error(struct wl_display *display,
			      const struct wl_interface **interface,
			      uint32_t *id);
-}

-- | Write to a pointer if it's not NULL
pokeNull :: Storable a => Ptr a -> a -> IO ()
pokeNull ptr val
  | ptr == nullPtr = return ()
  | otherwise      = poke ptr val

display_get_protocol_error :: StablePtr Proxy -> Ptr (Ptr WL_interface) -> Ptr CUInt -> IO CUInt
display_get_protocol_error proxy ptrIface ptrId  = do
  proxyVal <- deRefStablePtr proxy
  err <- readTVarIO $ displayErr $ proxyDisplayData proxyVal
  case err of
    -- Should the first (ignored) number here be EPROTO?
    Just ( _ , Just (code , interface , objid)) -> do
      -- Handle C's awkward way of returning values
      pokeNull ptrIface interface
      pokeNull ptrId (fromIntegral objid)
      return $ fromIntegral code
    _ -> return 0

foreign export ccall "wl_display_get_protocol_error" display_get_protocol_error
  :: StablePtr Proxy -> Ptr (Ptr WL_interface) -> Ptr CUInt -> IO CUInt

{-
int
wl_display_flush(struct wl_display *display);
-}

{-
int
wl_display_roundtrip_queue(struct wl_display *display,
			   struct wl_event_queue *queue);
-}

display_roundtrip_queue' :: Proxy -> EventQueue -> IO CInt
display_roundtrip_queue' = error "roundtrip undefined" -- TODO implement

{-
int
wl_display_roundtrip(struct wl_display *display);
-}

display_roundtrip :: StablePtr Proxy -> IO CInt
display_roundtrip proxy = do
  proxyVal <- deRefStablePtr proxy
  queue <- deRefStablePtr $ displayDefaultQueue $ proxyDisplayData proxyVal
  -- TODO optimize the following: shouldn't need to deRef the StablePtrs again
  display_roundtrip_queue' proxyVal queue

foreign export ccall "wl_display_roundtrip" display_roundtrip
  :: StablePtr Proxy -> IO CInt

{-
struct wl_event_queue *
wl_display_create_queue(struct wl_display *display);
-}

display_create_queue :: StablePtr Proxy -> IO (StablePtr EventQueue)
display_create_queue _ = newTQueueIO >>= newStablePtr

foreign export ccall "wl_display_create_queue" display_create_queue
  :: StablePtr Proxy -> IO (StablePtr EventQueue)

{-
int
wl_display_prepare_read_queue(struct wl_display *display,
			      struct wl_event_queue *queue);
-}

{-
int
wl_display_prepare_read(struct wl_display *display);
-}

{-
void
wl_display_cancel_read(struct wl_display *display);
-}

{-
int
wl_display_read_events(struct wl_display *display);
-}

{-
void
wl_log_set_handler_client(wl_log_func_t handler);
-}
