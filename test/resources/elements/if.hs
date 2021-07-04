module Main (main) where

testIf1 p x = if p then x else 10

main = do
  putStrLn $ show $ testIf1 True 1
  putStrLn $ show $ testIf1 False undefined
