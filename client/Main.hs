module Main where

import Echo.Client
import Network.Socket (close, withSocketsDo)
import qualified Control.Exception as E


main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "3000"
  E.bracket (open addr) close talk
