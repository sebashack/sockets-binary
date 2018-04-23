module Echo.Server where

import Control.Monad (forever, void, unless)
import Control.Concurrent (forkFinally)
import Network.Socket ( defaultHints
                      , getAddrInfo
                      , socket
                      , setSocketOption
                      , bind
                      , listen
                      , accept
                      , close
                      , AddrInfo(..)
                      , AddrInfoFlag(..)
                      , SocketOption(..)
                      , SocketType(..)
                      , Socket
                      , ServiceName )
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as S


resolve :: ServiceName -> IO AddrInfo
resolve port = do
  let hints :: AddrInfo
      hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  (addr:_) <- getAddrInfo (Just hints) Nothing (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  bind sock (addrAddress addr)
  listen sock 10
  return sock

loop :: Socket -> IO ()
loop sock = forever $ do
  (conn, peer) <- accept sock
  putStrLn $ "Connection from " ++ show peer
  void $ forkFinally (talk conn) (\_ -> close conn)

talk :: Socket -> IO ()
talk conn = do
  msg <- recv conn 1024
  unless (S.null msg) $ do
    sendAll conn msg
    talk conn
