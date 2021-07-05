module Map where

{-

f _ [] = error "x"
f Nothing x = x
f (Just x) xs = x + 2

f x y =
  case x of
    _ -> case x of 

-}
map' _ [] = []
map' f (x:xs) = f x : map' f xs

main = putStrLn "ok"
