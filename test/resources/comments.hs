{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Main {-# DEPRECATED "Comments are highly depricated" #-} where

{- First -}
{-# WARNING test "Don't use it!" #-}
test = 1

main = putStrLn $ show test
