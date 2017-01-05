{-|
Module      : Graphics.Sudbury.Socket
Description : Sets up wayland client/server sockets
Copyright   : (c) Auke Booij, 2015-2017
License     : MIT
Maintainer  : auke@tulcod.com
Stability   : experimental
Portability : POSIX
-}
module Graphics.Sudbury.Socket where

import Data.Maybe (fromMaybe)
import System.Environment (getEnv)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import System.Posix.IO (setFdOption, FdOption(..))
import System.Posix.Types (Fd(..))
import qualified Network.Socket as S hiding (send, sendTo, recv, recvFrom)

findServerSocket :: IO FilePath
findServerSocket = do
  dir <- getEnv "XDG_RUNTIME_DIR"
  socketName <- getEnv "WAYLAND_DISPLAY"
  return $ dir </> socketName

-- | As per the C implementation: XDG_RUNTIME_DIR is obligatory, but WAYLAND_DISPLAY is optional
findServerSocketWithDefault :: IO FilePath
findServerSocketWithDefault = do
  dir <- getEnv "XDG_RUNTIME_DIR"
  socketName <- catchIOError (getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")
  return $ dir </> socketName

findServerSocketWithName :: String -> IO FilePath
findServerSocketWithName name = do
  dir <- getEnv "XDG_RUNTIME_DIR"
  return $ dir </> name

-- | This is like 'connect_to_socket' in wayland-client.c
findServerSocketWithName' :: Maybe String -> IO FilePath
findServerSocketWithName' mname = do
  dir <- getEnv "XDG_RUNTIME_DIR"
  socketName <- catchIOError (getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")
  let name = fromMaybe socketName mname
  return $ dir </> name

makeClosingSocket :: IO S.Socket
makeClosingSocket = do
  socket <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
  setFdOption (Fd $ S.fdSocket socket) CloseOnExec True
  return socket

-- | Create a socket, and connect it to a wayland server
connectToServer :: FilePath -> IO S.Socket
connectToServer path = do
  socket <- makeClosingSocket
  S.connect socket (S.SockAddrUnix path)
  return socket

connectWithDefault :: IO S.Socket
connectWithDefault = do
  path <- findServerSocketWithDefault
  connectToServer path

bindAsServer :: FilePath -> IO S.Socket
bindAsServer path = do
  socket <- makeClosingSocket
  S.bind socket (S.SockAddrUnix path)
  return socket

bindAsServerWithDefault :: IO S.Socket
bindAsServerWithDefault = do
  path <- findServerSocketWithDefault
  bindAsServer path
