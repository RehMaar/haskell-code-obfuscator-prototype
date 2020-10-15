module Main where


testDo1 = do
    putStrLn "Privet"
    b <- return "Da"
    putStrLn (b ++ "?")
    let a = "A u mena normal'no"
    putStrLn a

testDo2 = do putStrLn "Hi!"

main = do
  testDo1
  testDo2
