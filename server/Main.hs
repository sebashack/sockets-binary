module Main where

import Echo.Server
import Network.Socket (close, withSocketsDo)
import qualified Control.Exception as E


main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "3000"
  E.bracket (open addr) close loop
