module Main where

testList_1 = [1, 2, 3, 4, 5]
testList_2 x y z = [x, x + y, case x of { 0 -> y; _ -> z}]
testList_3 = [(i, j) | i <- [1..], j <- [1..]]

main = do
  putStrLn $ show testList_1
  putStrLn $ show $ testList_2 1 2 3
  putStrLn $ show $ take 3 $ testList_3
