module Main where

testCase :: Int -> [Int]
testCase x = case x of
               0 -> []
               x -> x : testCase (pred x)

main = do
  putStrLn $ show $ testCase 0
  putStrLn $ show $ testCase 3
