module Main where

testStr1 = "Privet"
testStr2 = 'c' : "at"
testStr3 = ['c', 'a', 't']
testStr4 = "Привет!э"

testStr5 "a" = "b"
testStr5 _ = "c"

main = do
  putStrLn testStr1
  putStrLn testStr2
  putStrLn testStr3
  putStrLn testStr4
  putStrLn $ testStr5 "hi"
