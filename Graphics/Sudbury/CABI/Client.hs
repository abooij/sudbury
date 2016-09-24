{-|
Module      : Graphics.Sudbury.CABI.Client
Description : C ABI
Copyright   : (c) Auke Booij, 2015
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental

We attempt to implement the ABI of the libwayland C library.
This implements the server-side ABI.
-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Graphics.Sudbury.CABI.Client where

import qualified Data.ByteString as B
import Control.Monad (foldM_)
import Data.Word
import Data.Int
import Data.Maybe (fromMaybe , catMaybes)
import qualified Data.IntMap as IM
import Control.Concurrent.STM
import Foreign.C
import Foreign.C.Error
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.Posix.Types (Fd(..))
import Network.Socket (fdSocket)
import Data.Attoparsec.ByteString
import System.IO.Unsafe

import Graphics.Sudbury.Argument
import Graphics.Sudbury.Socket
import Graphics.Sudbury.Socket.Wayland
import Graphics.Sudbury.WirePackages
import Graphics.Sudbury.WireMessages
import Graphics.Sudbury.Message
import Graphics.Sudbury.Lifetime

import Graphics.Sudbury.CABI.DispatchFFI
import Graphics.Sudbury.CABI.Common
import Graphics.Sudbury.CABI.Structs


{-
TODO

- figure out which pointer arguments can be NULL
- figure out which error conditions should be properly forwarded
- match C code in wayland-client.c with libwayland code to track changes
- minimize C code
- minimize pointer dereferencing
- rename proxy and proxyVal to proxyPtr and proxy
-}


type EventQueue = (MessageQueue , FdQueue)

data Callback = HaskCall HaskDispatcher
              | FFICalls Listener
              | DispCall CDispatcher

data Proxy = Proxy
  { proxyDisplayData :: DisplayData
  , proxyDisplay :: Proxy
  , proxyQueue :: TVar (StablePtr EventQueue)
  , proxyListener :: TVar (Maybe Callback)  -- ^ listener/implementation or dispatcher (with data)
  , proxyUserData :: TVar UserData
  , proxyId :: Word32
  , proxyInterface :: Ptr WL_interface
  , proxyVersion :: Word32
  -- flags: destroyed, id_deleted?
  --
  }

data DisplayData = DisplayData
  { displayInFd :: TMVar Fd
    -- ^ fd to read from
  , displayOutFd :: TMVar Fd
    -- ^ fd to write to
  , displayErr :: TVar (Maybe (Int32, Maybe (Word32 , Ptr WL_interface , Word32)))
    -- ^ last_error, code, interface, id
  , displayDefaultQueue :: StablePtr EventQueue
    -- ^ display_queue should be the proxy's queue
  , displayOutQueue :: MessageQueue
    -- ^ messages to write to the fd
  , displayFdQueue :: FdQueue
    -- ^ fd's to be copied to the Other Side (tm)
  , displayLifetime :: Lifetime (StablePtr Proxy)
    -- ^ object lifetime tracker

  -- mutex?
  -- reader_cond?
  -- read_serial?
  -- reader_count?
  }

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

foreign import ccall unsafe "wl_os_dupfd_cloexec" os_dupfd_cloexec :: Fd -> CLong -> IO Fd
cArgToWireArg :: SArgumentType t -> CArgument t -> IO (WireArgument t , Maybe Fd)
cArgToWireArg SIntWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SUIntWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SFixedWAT n = return (fromIntegral n , Nothing)
cArgToWireArg SStringWAT cstr
  | cstr == nullPtr = return (B.empty , Nothing)
  | otherwise       =
    do
    len <- lengthArray0 0 cstr
    bs <- B.packCStringLen (cstr , len + 1)
    return (bs , Nothing)
cArgToWireArg SObjectWAT proxy
  | proxy == nullPtr = return (0 , Nothing)
  | otherwise        =
    do
    proxyVal <- deRefStablePtr $ castPtrToStablePtr proxy
    return (proxyId proxyVal , Nothing)
cArgToWireArg SNewIdWAT proxy
  | proxy == nullPtr = return (0 , Nothing)
  | otherwise        =
    do
    proxyVal <- deRefStablePtr $ castPtrToStablePtr proxy
    return (proxyId proxyVal , Nothing)
cArgToWireArg SArrayWAT ar = do
  array <- peek ar
  bs <- B.packCStringLen (arrayData array , fromIntegral $ arraySize array)
  return (bs , Nothing)
cArgToWireArg SFdWAT fd = do
  newfd <- os_dupfd_cloexec fd 0
  return (() , Just newfd)
  -- here we should duplicate the Fd and return it in the second argument

-- this is a hack with STM
-- TODO synchronize properly
getWireArg :: Ptr WL_arg -> SArgumentType t -> Word32 -> IO (WireArgument t , Maybe Fd)
getWireArg _ SNewIdWAT pid = return (pid , Nothing)
getWireArg ptr x _ = do
  cArg <- readCArg ptr x
  cArgToWireArg x cArg

haskInvoker :: Listener -> StablePtr Proxy -> Word16 -> [CArgBox] -> IO ()
haskInvoker list proxy opcode args = do
  fun <- peekElemOff list (fromIntegral opcode)
  proxyVal <- deRefStablePtr proxy
  udata <- readTVarIO $ proxyUserData proxyVal
  invokeFFI fun udata (castStablePtrToPtr proxy) args

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

proxy_marshal_array :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> IO ()
proxy_marshal_array proxy opcode args = do
  proxyVal <- deRefStablePtr proxy
  iface <- peek (proxyInterface proxyVal)
  msg <- peekElemOff (ifaceMethods iface) (fromIntegral opcode)
  argTypes <- signatureToTypes (msgSignature msg)
  -- TODO the following two expressions should be made prettier
  let ptrs = iterate (flip plusPtr wl_arg_size) args
  haskArgs <- sequence $ zipWith (
    \(ArgTypeBox x) ptr -> do
      (a , b) <- getWireArg ptr x (error "Unexpected new_id argument!")
      -- passing error as new_id here since this message should not have new_id arguments
      return (WireArgBox x a , b)
    ) argTypes ptrs
  let wireMsg = WireMessage
       { wireMessageSender = (proxyId proxyVal)
       , wireMessageOpcode = fromIntegral opcode
       , wireMessageArguments = fst $ unzip haskArgs
       }
  let fds = catMaybes $ snd $ unzip haskArgs
  let dd = proxyDisplayData proxyVal
      out_queue = displayOutQueue dd
      fd_queue  = displayFdQueue dd
  atomically $ do
    writeTQueue out_queue wireMsg
    mapM_ (writeTQueue fd_queue) fds
  -- TODO actually send data (as in libwayland)
  return ()

foreign export ccall "wl_proxy_marshal_array" proxy_marshal_array
  :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> IO ()

{-
struct wl_proxy *
wl_proxy_create(struct wl_proxy *factory,
		const struct wl_interface *interface);
-}

proxy_create :: StablePtr Proxy -> Word32 -> Ptr WL_interface -> Word32 -> IO (StablePtr Proxy)
proxy_create factory pid interface version = do
  factVal <- deRefStablePtr factory
  queueVar <- atomically $ do
    factQueue <- readTVar (proxyQueue factVal)
    newTVar factQueue
  listenerVar <- newTVarIO Nothing
  dataVar <- newTVarIO nullPtr

  newStablePtr $ Proxy
    { proxyDisplay = proxyDisplay factVal
    , proxyDisplayData = proxyDisplayData factVal
    , proxyQueue = queueVar
    , proxyListener = listenerVar
    , proxyUserData = dataVar
    , proxyId = pid
    , proxyInterface = interface
    , proxyVersion = version
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
wl_proxy_marshal_constructor_versioned(struct wl_proxy *proxy,
                                       uint32_t opcode,
                                       const struct wl_interface *interface,
                                       uint32_t version,
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
proxy_marshal_array_constructor factoryP opcode args interface =
  withStablePtr' factoryP $ \factory ->
    proxy_marshal_array_constructor_versioned factoryP opcode args interface (fromIntegral $ proxyVersion factory)

foreign export ccall "wl_proxy_marshal_array_constructor" proxy_marshal_array_constructor
  :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> Ptr WL_interface -> IO (StablePtr Proxy)

{-
struct wl_proxy *
wl_proxy_marshal_array_constructor_versioned(struct wl_proxy *proxy,
                                             uint32_t opcode,
                                             union wl_argument *args,
                                             const struct wl_interface *interface,
                                             uint32_t version);
-}

proxy_marshal_array_constructor_versioned :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> Ptr WL_interface -> CUInt -> IO (StablePtr Proxy)
proxy_marshal_array_constructor_versioned factory opcode args interface version = do
  factVal <- deRefStablePtr factory
  let dd = proxyDisplayData factVal
  -- FIXME STM the following
  pid <- atomically $ allocId (displayLifetime dd)
  proxy <- proxy_create factory pid interface (fromIntegral version)
  -- TODO execute in STM rather than IO
  atomically $ localCreate (displayLifetime dd) pid proxy
  iface <- peek (proxyInterface factVal)
  msg <- peekElemOff (ifaceMethods iface) (fromIntegral opcode)
  argTypes <- signatureToTypes (msgSignature msg)
  -- TODO the following two expressions should be made prettier
  let ptrs = iterate (flip plusPtr wl_arg_size) args
  haskArgs <- sequence $ zipWith (
    \(ArgTypeBox x) ptr -> do
      (a , b) <- getWireArg ptr x pid
      return (WireArgBox x a , b)
    ) argTypes ptrs
  let wireMsg = WireMessage
       { wireMessageSender = (proxyId factVal)
       , wireMessageOpcode = fromIntegral opcode
       , wireMessageArguments = fst $ unzip haskArgs
       }
  let fds = catMaybes $ snd $ unzip haskArgs
  let --dd = proxyDisplayData factVal
      out_queue = displayOutQueue dd
      fd_queue  = displayFdQueue dd
  atomically $ do
    writeTQueue out_queue wireMsg
    mapM_ (writeTQueue fd_queue) fds
  -- TODO actually send data (as in libwayland)
  return proxy

foreign export ccall "wl_proxy_marshal_array_constructor_versioned" proxy_marshal_array_constructor_versioned
  :: StablePtr Proxy -> CUInt -> Ptr WL_arg -> Ptr WL_interface -> CUInt -> IO (StablePtr Proxy)

{-
void
wl_proxy_destroy(struct wl_proxy *proxy);
-}

-- this function is dangerous for values of type Ptr Proxy that were not created using wl_proxy_create
-- (but this is nothing new w.r.t. the C side: don't destroy stuff you didn't create)
proxy_destroy :: StablePtr Proxy -> IO ()
proxy_destroy proxyPtr = do
  proxy <- deRefStablePtr proxyPtr
  let dd = proxyDisplayData proxy
  -- Here, libwayland differentiates between client-side and server-side objects
  -- and acts differently according to that. We need to figure out the purpose of that.
  atomically $ localDestroy (displayLifetime dd) (proxyId proxy)
  freeStablePtr proxyPtr
  -- TODO free id? or should that be done in the delete_id handler?

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
    written <- writeMaybeTVar' (proxyListener proxyVal) (FFICalls listener)
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
    Just (FFICalls x) -> x
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
    written <- writeMaybeTVar' (proxyListener proxyVal) (DispCall (dispatcher , ddata))
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
wl_proxy_get_version(struct wl_proxy *proxy);
-}


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

newEventQueueIO :: IO EventQueue
newEventQueueIO = do
  msgs <- newTQueueIO
  fds <- newTQueueIO
  return (msgs , fds)

display_connect :: CString -> IO (StablePtr Proxy)
display_connect cstr = do
  str <- peekCStringMaybe cstr

  sock <- findServerSocketWithName' str >>= connectToServer
  let fd = Fd $ fdSocket sock
  err <- newTVarIO Nothing
  default_queue <- newEventQueueIO >>= newStablePtr
  out_queue <- newTQueueIO
  fd_queue <- newTQueueIO
  in_fd_var <- newTMVarIO fd
  out_fd_var <- newTMVarIO fd
  lifetime <- initLifetimeIO

  let dd = DisplayData
        { displayInFd = in_fd_var
        , displayOutFd = out_fd_var
        , displayErr = err
        , displayDefaultQueue = default_queue
        , displayOutQueue = out_queue
        , displayFdQueue = fd_queue
        , displayLifetime = lifetime
        }

  queue <- newTVarIO default_queue
  listener <- newTVarIO Nothing
  udata <- newTVarIO nullPtr
  let proxy = Proxy
        { proxyDisplayData = dd
        , proxyDisplay = proxy
        , proxyQueue = queue
        , proxyListener = listener
        , proxyUserData = udata
        , proxyId = 1
        , proxyInterface = display_interface
        , proxyVersion = 0
        }

  ptr <- newStablePtr proxy
  atomically $ localCreate lifetime 1 ptr
  return ptr

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
  fd <- atomically $ readTMVar $ displayInFd $ proxyDisplayData proxyVal
  return fd

foreign export ccall "wl_display_get_fd" display_get_fd
  :: StablePtr Proxy -> IO Fd

{-
int
wl_display_dispatch(struct wl_display *display);
-}

display_dispatch :: StablePtr Proxy -> IO CInt
display_dispatch proxy = do
  proxyVal <- deRefStablePtr proxy
  display_dispatch_queue proxy (displayDefaultQueue $ proxyDisplayData proxyVal)

foreign export ccall "wl_display_dispatch" display_dispatch
  :: StablePtr Proxy -> IO CInt

{-
int
wl_display_dispatch_queue(struct wl_display *display,
			  struct wl_event_queue *queue);
-}

-- C

foreign import ccall "wl_display_dispatch_queue" display_dispatch_queue
  :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt

{-
int
wl_display_dispatch_queue_pending(struct wl_display *display,
				  struct wl_event_queue *queue);
-}

{-
given: a -> (b -> c) -> c
want: [a] -> ([b] -> c) -> c
[] f = f []
(x:xs) f = with x $ \x' -> rec xs (\l -> f (x':l))
-}

makeHandler :: ((b -> c) -> a -> c) -> ([b] -> c) -> [a] ->  c
makeHandler _ f [] = f []
makeHandler h f (x:xs) = h (\x' -> makeHandler h (\l -> f (x':l)) xs) x

display_dispatch_queue_pending' :: Proxy -> EventQueue -> IO CInt
display_dispatch_queue_pending' proxy (events , fds) = do
  let dd = proxyDisplayData proxy
  event <- atomically $ tryReadTQueue events
  case event of
    Nothing -> return 0
    Just msg -> do
      let sender = wireMessageSender msg
          opcode = wireMessageOpcode msg
      withLocalObjectIO (displayLifetime dd) sender $ \senderobj -> do
        senderval <- deRefStablePtr senderobj
        listener <- readTVarIO (proxyListener senderval)
        case listener of
          Nothing -> return ()
          Just (HaskCall _) -> error "HaskCall not implemented"
          Just (FFICalls li) -> do
            imp <- peekElemOff li (fromIntegral opcode)
            udata <- readTVarIO (proxyUserData senderval)
            makeHandler
              (handleWireCArg
                -- TODO hack
                (\pid -> unsafePerformIO $ do
                    maybePtr <- withLocalObjectIO (displayLifetime dd) pid (return . castStablePtrToPtr)
                    return $ fromMaybe nullPtr maybePtr
                )
                fds
              )
              (\cargs -> invokeFFI imp udata (castStablePtrToPtr senderobj) cargs)
              (wireMessageArguments msg)
          Just (DispCall _) -> error "DispCall not implemented"
      display_dispatch_queue_pending' proxy (events , fds)

display_dispatch_queue_pending :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt
display_dispatch_queue_pending ptrP ptrE =
  withStablePtr' ptrP $ \proxy ->
    withStablePtr' ptrE $ \queue ->
      display_dispatch_queue_pending' proxy queue

foreign export ccall "wl_display_dispatch_queue_pending" display_dispatch_queue_pending
  :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt

{-
int
wl_display_dispatch_pending(struct wl_display *display);
-}

display_dispatch_pending :: StablePtr Proxy -> IO CInt
display_dispatch_pending = withStablePtr $ \proxy -> do
  default_queue <- deRefStablePtr $ displayDefaultQueue $ proxyDisplayData proxy
  display_dispatch_queue_pending' proxy default_queue

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

display_flush :: StablePtr Proxy -> IO CInt
display_flush proxy = do
  proxyVal <- deRefStablePtr proxy
  let dd = proxyDisplayData proxyVal
      fdVar = displayOutFd dd
  fd <- atomically $ takeTMVar fdVar

  (outData , outFds) <- atomically $ do
    bytes <- serializeQueue (displayOutQueue dd)
    fds <- takeFds (displayFdQueue dd)
    return (bytes , fds)
  sent <-
    if B.length outData == 0
      then
        return 0
      else do
        sendToWayland fd outData outFds
  atomically $ putTMVar fdVar fd
  return sent

foreign export ccall "wl_display_flush" display_flush
  :: StablePtr Proxy -> IO CInt

{-
int
wl_display_roundtrip_queue(struct wl_display *display,
			   struct wl_event_queue *queue);
-}

-- C
foreign import ccall "wl_display_roundtrip_queue" display_roundtrip_queue
  :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt

{-
int
wl_display_roundtrip(struct wl_display *display);
-}

display_roundtrip :: StablePtr Proxy -> IO CInt
display_roundtrip proxy = do
  proxyVal <- deRefStablePtr proxy
  let queue = displayDefaultQueue $ proxyDisplayData proxyVal
  -- TODO optimize the following: shouldn't need to deRef the StablePtrs again
  display_roundtrip_queue proxy queue

foreign export ccall "wl_display_roundtrip" display_roundtrip
  :: StablePtr Proxy -> IO CInt

{-
struct wl_event_queue *
wl_display_create_queue(struct wl_display *display);
-}

display_create_queue :: StablePtr Proxy -> IO (StablePtr EventQueue)
display_create_queue _ = newEventQueueIO >>= newStablePtr

foreign export ccall "wl_display_create_queue" display_create_queue
  :: StablePtr Proxy -> IO (StablePtr EventQueue)

{-
int
wl_display_prepare_read_queue(struct wl_display *display,
			      struct wl_event_queue *queue);
-}

-- Foreign.C.Error would've been great if they had exposed this symbol...
foreign import ccall unsafe "HsBase.h __hscore_set_errno" set_errno :: Errno -> IO ()

-- | Implement wayland's locking scheme.
display_prepare_read_queue :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt
display_prepare_read_queue _ queue = do
  -- This scheme only works because of the assumption that
  -- at most one (posix) thread reads from a given queue.
  (queueVal , _) <- deRefStablePtr queue
  empty <- atomically $ isEmptyTQueue queueVal
  case empty of
    True -> return 0
    False -> do
      set_errno eAGAIN
      return (-1)

foreign export ccall "wl_display_prepare_read_queue" display_prepare_read_queue
  :: StablePtr Proxy -> StablePtr EventQueue -> IO CInt

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

arg_pipe_fd :: FdQueue -> FdQueue -> SArgumentType t -> STM ()
arg_pipe_fd popQueue pushQueue SFdWAT = readTQueue popQueue >>= writeTQueue pushQueue
arg_pipe_fd _ _ _ = return ()

queue_package :: DisplayData -> FdQueue -> WirePackage -> IO (Maybe ())
queue_package dd fd_queue pkg =
  let sender = wirePackageSender pkg
      opcode = wirePackageOpcode pkg
  in
  withLocalObjectIO (displayLifetime dd) sender $ \proxyPtr -> do
    proxy <- deRefStablePtr proxyPtr
    (events , fds) <- (readTVarIO $ proxyQueue proxy) >>= deRefStablePtr
    iface <- peek (proxyInterface proxy)
    wlmsg <- peekElemOff (ifaceEvents iface) (fromIntegral opcode)
    let signature = msgSignature wlmsg
    types <- signatureToTypes signature
    let res = parseOnly (payloadFromTypes sender opcode types) (wirePackagePayload pkg)
    msg <- case res of
      Left err -> do
        error ("Message parse failed: " ++ err)
      Right x -> return x
    let dd = proxyDisplayData proxy
        version = proxyVersion proxy -- by default, proxies get the version of their parent
        storeNewId :: Ptr (Ptr WL_interface) -> Int -> WireArgBox -> IO Int
        storeNewId ptr idx (WireArgBox SNewIdWAT n) = do
          iface <- peekElemOff ptr idx
          newProxy <- proxy_create proxyPtr n iface version
          atomically $ foreignCreate (displayLifetime dd) n newProxy
          return $ idx + 1
        storeNewId _ idx (WireArgBox _ _) = return idx
    foldM_ (storeNewId (msgInterfaces wlmsg)) 0 (wireMessageArguments msg)
    atomically $ do
      -- Store this package in the right queue, and give that queue the right number of Fds
      writeTQueue events msg
      mapM_ (\(ArgTypeBox x) -> arg_pipe_fd fd_queue fds x) types

pop_fd :: WireMessage -> FdQueue-> STM Message
pop_fd msg queue = do
  args <- mapM (\(WireArgBox x a) -> ArgBox x <$> (read_message_arg x a)) (wireMessageArguments msg)
  return Message
    { messageSender = wireMessageSender msg
    , messageOpcode = wireMessageOpcode msg
    , messageArguments = args
    }
    where
      read_message_arg :: SArgumentType t -> WireArgument t -> STM (UnboxedArgument t)
      read_message_arg SIntWAT n = return n
      read_message_arg SUIntWAT n = return n
      read_message_arg SFixedWAT n = return n
      read_message_arg SStringWAT str = return str
      read_message_arg SObjectWAT o = return o
      read_message_arg SNewIdWAT n = return n
      read_message_arg SArrayWAT a = return a
      read_message_arg SFdWAT _ = readTQueue queue

-- | implement wayland's locking scheme for fd reading, and read from the fd
display_read_events :: StablePtr Proxy -> IO CInt
display_read_events proxyPtr = do
  proxy <- deRefStablePtr proxyPtr
  let dd = proxyDisplayData proxy
  out <- atomically $ do
    err <- readTVar $ displayErr dd
    case err of
      Just (errNo , _) -> return $ Left errNo
      Nothing -> do
        -- FIXME this locking is too restrictive: might result in deadlock
        fd <- tryTakeTMVar (displayInFd dd)
        return $ Right fd
  case out of
    Left errNo -> do
      set_errno (Errno $ fromIntegral errNo)
      return (-1)
    Right (Just fd) -> do
      (bytes , fds) <- recvFromWayland fd

      fd_queue <- newTQueueIO
      atomically $ mapM_ (writeTQueue fd_queue) fds

      let pkgsParse = parseOnly pkgStream bytes
      pkgs <-
        case pkgsParse of
          Left err -> do
            error ("Package parse failed: " ++ err)
          Right x -> return x
      mapM_ (queue_package dd fd_queue) pkgs

      atomically $ putTMVar (displayInFd dd) fd
      return 0
    Right Nothing -> do
      -- wait until the fd is returned (ie our queue is populated)
      -- FIXME this is a sequential thread synchronization.
      -- libwayland does this better (using pthread_cond_wait)
      _ <- atomically $ readTMVar (displayInFd dd)
      err <- atomically $ readTVar $ displayErr dd
      -- i'm not sure why we're setting errno multiple times, but this is what libwayland does
      -- FIXME sort this out
      case err of
        Just (errNo , _) -> do
          set_errno (Errno $ fromIntegral errNo)
          return (-1)
        Nothing ->
          return 0

foreign export ccall "wl_display_read_events" display_read_events
  :: StablePtr Proxy -> IO CInt

{-
void
wl_log_set_handler_client(wl_log_func_t handler);
-}

log_set_handler_client :: Ptr () -> IO ()
log_set_handler_client _ = return ()

foreign export ccall "wl_log_set_handler_client" log_set_handler_client
  :: Ptr () -> IO ()
