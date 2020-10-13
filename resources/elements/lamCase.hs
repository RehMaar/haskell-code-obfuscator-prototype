module Main where

testCase = \x y z -> case x of
                      0 -> []
                      x -> x : testCase (pred x) y z
main = do
  putStrLn $ show $ testCase 5 undefined undefined
