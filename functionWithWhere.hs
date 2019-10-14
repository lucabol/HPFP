module FunctionWithWhere where

printInc n = print plusTwo
    where plusTwo = n + 2

mul1 = x * 3 + y where
  x  = 3
  y  = 1000

mul2 = z / x + y where
    x = 7
    y = -x
    z = y