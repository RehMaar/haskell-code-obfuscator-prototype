module T4 where

(#) :: Int -> Int -> Int
(#) a b = a + b

test4 _ [] = Nothing
test4 p (x:xs)
  | p x
  = Just (x # x)
  | otherwise
  = test4 p xs

f = \x -> x
