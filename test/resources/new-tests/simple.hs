module Simple (main) where

import Data.Maybe

testStrings = "Hello!"
testChar = 'c'

testOperations = 1 + 2 - 3 * 10 / (12 * 32)

testLet = let a = 10; b = 20; x = 30; y = 40 in y * a - b + x

testArgs x y z = z + y + x

testIf [x] = if isJust x then fromJust x else -1
testIf _ = if True then 1 else 2

testCase y =
  case y of
    (x:xs) -> x + testCase xs
    [x] -> x * 2
    _ -> 0

testPats (Just (_:xs)) = xs
testPats (Just [x]) = [1, x]
testPats Nothing = []
testPats _ = [111]

testGuards x
  | isJust x
  = True
  | Nothing <- x
  , 1 == (20 / 20)
  = False
  | otherwise
  = True

main = do
  putStrLn $ show $ testOperations
  putStrLn $ show $ testLet
  putStrLn $ show $ testArgs 1 2 3
  putStrLn $ show $ testPats Nothing
  putStrLn $ show $ testPats $ Just [1, 2, 3, 4]
  putStrLn $ show $ testPats $ Just [1]
  putStrLn $ show $ testPats $ Just []
  putStrLn $ show $ testGuards (Just 1)
  putStrLn $ show $ testGuards Nothing
  putStrLn $ show $ testCase [1, 2, 3, 4, 5]
  putStrLn $ show $ testIf [Just 1]
  putStrLn $ show $ testIf []

