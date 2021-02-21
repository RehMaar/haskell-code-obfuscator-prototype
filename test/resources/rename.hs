module Rename (test1) where

test1 = 1
test2 x y = x + y
test3 x = test1 + x
test4 x = map (test2 1) x
  where test1 = 2
test5 = let x = 1
            y = 2
        in test2 x y

