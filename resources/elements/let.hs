module Main where

testLet1 x =
  let y = x * 20
      (p, f) = (1, 2)
  in p + f + y

main = do
  putStrLn $ show $ testLet1 2
