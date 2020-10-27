module Main where


testDo1 = do
    putStrLn "Privet"
    b <- return "Da"
    let a = "A u mena normal'no"
    let Just (c, b) | b > 2 = Just (10, 20)
                    | otherwise = Just (0, 0)
    putStrLn $ show c ++ " " ++ show b

do
 let Just a = b + 2
     ..
=>
(\(Just a)-> {}) (let newName a = a + 2 in newName)


testDo2 = do putStrLn "Hi!"

main = do
  testDo1
  testDo2
