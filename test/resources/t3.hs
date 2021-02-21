module T3 where

test2 0 = 0
test2 n | n < 5 = let y = 12
                      x = 13
                      z = 15
                  in n + y + z + x
        | n < 10 = 123 * n
        | otherwise = n - 2
