module Main (main, f) where

import Data.List
import Data.Maybe (isJust)
import T

{- Hello, Comment, my old friend -}

(<&>) = (+)
x <^> y = x <&> y

h [] = 0
h (x:xs)
  | isJust x = 1 + h xs
  | otherwise = 2 + h xs

f x = 10

test2 x y = 12

test1 x = let y = 12 in 3 `test2` (f $ Main.f $ (+) x y)
  where
      f = \x -> case x of { 0 -> 0; x -> 10 * x }

      h = (+) 1 ((+) 2 3)

      (a, b) = (1, 2)

main = do
  putStrLn $ show $ 1 <^> 2
  putStrLn $ show $ h [Just 1, Nothing, Just 2, Nothing, Just 100]
  putStrLn $ show $ f undefined
  putStrLn $ show $ test2 undefined (error "A")
  putStrLn $ show $ test1 134
