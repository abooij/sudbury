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

import Data.Word
import Data.Int
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM
import Foreign.C
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import System.Posix.Types (Fd(..))

import Graphics.Sudbury.Crap.Common
import Graphics.Sudbury.Crap.Structs

-- stubs
data TODO
type MyEvent = TODO

type EventQueue = TQueue MyEvent

data Proxy = Proxy
  { proxyParent :: Either (StablePtr Proxy) (StablePtr DisplayData) -- ^ parent or wl_display data
  , proxyDisplay :: StablePtr Proxy
  , proxyQueue :: TVar (StablePtr EventQueue)
  , proxyListener :: TVar (Maybe (Either Listener Dispatcher))  -- ^ listener/implementation or dispatcher (with data)
  , proxyUserData :: TVar UserData
  , proxyId :: Word32
  , proxyInterface :: WL_interface
  -- flags: destroyed, id_deleted?
  --
  }

data DisplayData = DisplayData
  { displayFd :: Fd
  , displayErr :: TVar (Maybe (Int32, Maybe (Word32 , Ptr WL_interface , Word32)))
    -- ^ last_error, code, interface, id
  , displayDefaultQueue :: StablePtr EventQueue -- display_queue should be the proxy's queue
  -- mutex?
  -- reader_cond?
  -- read_serial?
  -- reader_count?
  }

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
  proxyId <- undefined -- TODO
  interfaceVar <- peek interface
  newStablePtr $ Proxy
    { proxyParent = Left factory
    , proxyDisplay = proxyDisplay factVal
    , proxyQueue = queueVar
    , proxyListener = listenerVar
    , proxyUserData = dataVar
    , proxyId = proxyId
    , proxyInterface = interfaceVar
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

-- TODO

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
  case (proxyParent proxyVal) of
    Left _  -> return (-1) -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      return (displayFd ddVal)

foreign export ccall "wl_display_get_fd" display_get_fd
  :: StablePtr Proxy -> IO Fd

{-
int
wl_display_dispatch(struct wl_display *display);
-}

display_dispatch :: StablePtr Proxy -> IO CInt
display_dispatch proxy = do
  proxyVal <- deRefStablePtr proxy
  case (proxyParent proxyVal) of
    Left _   -> return (-1) -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      default_queue <- deRefStablePtr (displayDefaultQueue ddVal)
      display_dispatch_queue' proxyVal default_queue

foreign export ccall "wl_display_dispatch" display_dispatch
  :: StablePtr Proxy -> IO CInt

{-
int
wl_display_dispatch_queue(struct wl_display *display,
			  struct wl_event_queue *queue);
-}

display_dispatch_queue' :: Proxy -> EventQueue -> IO CInt
display_dispatch_queue' = undefined -- TODO implement

{-
int
wl_display_dispatch_queue_pending(struct wl_display *display,
				  struct wl_event_queue *queue);
-}

display_dispatch_queue_pending' :: Proxy -> EventQueue -> IO CInt
display_dispatch_queue_pending' = undefined -- TODO implement

{-
int
wl_display_dispatch_pending(struct wl_display *display);
-}

display_dispatch_pending :: StablePtr Proxy -> IO CInt
display_dispatch_pending proxy = do
  proxyVal <- deRefStablePtr proxy
  case (proxyParent proxyVal) of
    Left _   -> return (-1) -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      default_queue <- deRefStablePtr (displayDefaultQueue ddVal)
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
  case (proxyParent proxyVal) of
    Left _ -> return 0 -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      atomically $ do
        err <- readTVar (displayErr ddVal)
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
  case proxyParent proxyVal of
    Left _ -> return 0 -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      err <- readTVarIO (displayErr ddVal)
      case err of
        -- Should the first (ignored) number here be EPROTO?
        Just ( _ , Just (code , interface , id)) -> do
          -- Handle C's awkward way of returning values
          pokeNull ptrIface interface
          pokeNull ptrId (fromIntegral id)
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
display_roundtrip_queue' = undefined -- TODO implement

{-
int
wl_display_roundtrip(struct wl_display *display);
-}

display_roundtrip :: StablePtr Proxy -> IO CInt
display_roundtrip proxy = do
  proxyVal <- deRefStablePtr proxy
  case proxyParent proxyVal of
    Left _ -> return (-1) -- proxy was not a wl_display
    Right dd -> do
      ddVal <- deRefStablePtr dd
      queue <- deRefStablePtr (displayDefaultQueue ddVal)
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
