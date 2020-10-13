module Main where

testLam1 xs = map (\x -> x + 2) xs
testLam2 xs = map (\(x, y) -> x + y) xs
testLam3 = \x y z -> x + y + z

main = do
  putStrLn $ show $ testLam1 [1, 2, 3]
  putStrLn $ show $ testLam2 [(1, 1), (1, 2)]
  putStrLn $ show $ testLam3 1 2 3
