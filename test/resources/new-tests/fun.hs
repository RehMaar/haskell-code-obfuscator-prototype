module Main where

import Data.Char

fun [] (Just x)
  | isLower x
  = 1
fun (x:xs) (Just y)
  | isUpper x
  , isLower y
  = 2
fun (x:xs) _
  | isUpper x
  = 3
fun _ _ = 0

main = do
  putStrLn $ show $ fun [] Nothing
  putStrLn $ show $ fun [] (Just 'a')
  putStrLn $ show $ fun ['X'] (Just 'a')
  putStrLn $ show $ fun ['X'] Nothing
  putStrLn $ show $ fun ['y'] Nothing
