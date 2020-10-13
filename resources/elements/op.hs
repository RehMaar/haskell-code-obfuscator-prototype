module Main where

testOp0 = -1 + 2
testOp1 x y = x + (- y)
testOp2 x y = x `mod` y
testOp3 x = succ $ pred $ x
testOp6 x = x + 1 + 2
testOp4 x = succ $ x + 1
testOp5 x = succ $ x + 1 + 2
testOp7 = 2 + 3 * 4 + 5
testOp8 = 2 + 3 * 4 * 5 + 6 + 7 * 1

main = do
   putStrLn "test op"
   putStrLn $ show $ testOp0
   putStrLn $ show $ testOp1 1 2
   putStrLn $ show $ testOp2 1 2
   putStrLn $ show $ testOp3 1
   putStrLn $ show $ testOp6 1
   putStrLn $ show $ testOp4 1
   putStrLn $ show $ testOp5 1
   putStrLn $ show $ testOp7
   putStrLn $ show $ testOp8
