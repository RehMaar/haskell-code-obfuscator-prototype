module Data
  ( d2
  , A
  , B(..)
  , C(MkC, c1))
  where

data A a = MkA { a1 :: a }

data B = MkB { b1 :: Int }

data C = MkC { c1 :: Int } | MkC2 { c2 :: Int }

data D = MkD { d1 :: Int, d2 :: Int }

test1 = d1
test2 = MkD { d1 = 1, d2 = 2}
test3 = d2
