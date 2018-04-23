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
import Network.Socket.ByteString.Lazy (recv, sendAll)
import Echo.Types
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as C

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
  sendAll sock (encode $ OpE Sum (IntE 1 ) (IntE 2))
  msg <- recv sock 1024
  putStr "Received: "
  print (decode msg :: Int)
