module Echo.Types where


data Operator = Sum
              | Mult
              deriving Show

data Exp = IntE Int
         | OpE Operator Exp Exp
         deriving Show
