module Main where

import Control.Monad (guard)

testDo1 = do
    putStrLn "Privet"
    b <- return "Da"
    let a = "A u mena normal'no"
-- This kind of code will loop
-- So we do not support it.
{-    let Just (c, b) | b > 2 = Just (10, 20)
                    | otherwise = Just (0, 0)-}
    let Just (c, b) = Just (10, 20)
    putStrLn $ show c ++ " " ++ show b

testDo2 = do putStrLn "Hi!"

testDo3 = do
    x <- [1,2,3]
    y <- [1,2,3]
    True <- return (x /= y)
    return (x,y)

testDo4 = do
    z <- [1..]
    x <- [1..z]
    y <- [x..z]
    guard (x^2 + y^2 == z^2)
    return (x, y, z)

main = do
  testDo1
  testDo2
  putStrLn $ show $ testDo3
  putStrLn $ show $ take 5 $ testDo4
