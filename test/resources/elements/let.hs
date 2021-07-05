module Main where

testLet1 x =
  let y = x * 20
      (p, f) = (1, 2)
  in p + f + y

testLet2 =
  let f x | x > 2 = 2
      f x = x + 2
  in f 12

main = do
  putStrLn $ show $ testLet1 2
