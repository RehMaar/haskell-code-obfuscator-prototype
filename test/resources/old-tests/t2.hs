module T2 () where

test2 0 = 0
test2 n | n < 5 = let y = 12 in n + y
        | n < 10 = 123 * n
        | otherwise = n - 2
