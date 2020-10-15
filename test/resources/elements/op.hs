module Main where

--        ((- 1) + 2)
testOp0 = -1 + 2
--           (x + ((- y)))
testOp1 x y = x + (- y)
--           (x `mod` y)
testOp2 x y = x `mod` y
--        (succ $ (pred $ x))
testOp3 x = succ $ pred $ x
--        ((x + 1) + 2)
testOp6 x = x + 1 + 2
--        (succ $ (x + 1))
testOp4 x = succ $ x + 1
--testO5C x = (succ $ ((x + 1) + 2))
testOp5 x = succ $ x + 1 + 2
--        ((2 + (3 * 4)) + 5)
testOp7 = 2 + 3 * 4 + 5
--testO8C = (((2 + ((3 * 4) * 5)) + 6) + (7 * 1))
testOp8 = 2 + 3 * 4 * 5 + 6 + 7 * 1
--        (putStrLn $ (show $ (take 3 $ [1 .. ])))
testOp9 = putStrLn $ show $ take 3 $ [1..]
--testOp9 = f $ h $ g 3 $ [1..]
--testOp11 = f $ h $ [1..]
--testO10C = (2 + ((3 * 4) * 5))
testOp10 = 2 + 3 * 4 * 5
-- testOp10 = 2 + 3 * 4 * 5 + 6

x <^> y = x + y

main = do
  -- (putStrLn $ (show $ (1 <^> 2)))
   putStrLn $ show $ 1 <^> 2
   putStrLn $ show $ testOp0
   putStrLn $ show $ testOp1 1 2
   putStrLn $ show $ testOp2 1 2
   putStrLn $ show $ testOp3 1
   putStrLn $ show $ testOp6 1
   putStrLn $ show $ testOp4 1
   putStrLn $ show $ testOp5 1
   putStrLn $ show $ testOp7
   putStrLn $ show $ testOp8
   putStrLn $ show $ testOp10
   testOp9
