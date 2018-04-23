module Echo.Eval where

import Echo.Types

eval :: Exp -> Int
eval (IntE n) = n
eval (OpE Sum exp1 exp2) = eval exp1 + eval exp2
eval (OpE Mult exp1 exp2) = eval exp1 * eval exp2
