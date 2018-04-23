{-# LANGUAGE OverloadedStrings #-}

module Echo.Client where

import Network.Socket ( defaultHints
                      , getAddrInfo
                      , socket
                      , connect
                      , AddrInfo(..)
                      , HostName
                      , SocketType(..)
                      , Socket
                      , ServiceName )
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C


resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints :: AddrInfo
      hints = defaultHints { addrSocketType = Stream }
  (addr:_) <- getAddrInfo (Just hints) (Just host) (Just port)
  return addr

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  return sock

talk :: Socket -> IO ()
talk sock = do
  sendAll sock "Hello, Socket!"
  msg <- recv sock 1024
  putStr "Received: "
  C.putStrLn msg
