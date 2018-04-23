module Echo.Types where

import Data.Binary


data Operator = Sum
              | Mult
              deriving Show

data Exp = IntE Int
         | OpE Operator Exp Exp
         deriving Show

instance Binary Operator where
  put Sum = put (0 :: Word16)
  put Mult = put (1 :: Word16)
  get = do
    t <- get :: Get Word16
    case t of
      0 -> return Sum
      1 -> return Mult

instance Binary Exp where
  put (IntE i) = do
    put (0 :: Word8)
    put i
  put (OpE op e1 e2) = do
    put (1 :: Word8)
    put op
    put e1
    put e2
  get = do
    t <- get :: Get Word8
    case t of
      0 -> get >>= \i -> return (IntE i)
      1 -> do
        op <- get :: Get Operator
        e1 <- get
        e2 <- get
        return (OpE op e1 e2)
