{-# LANGUAGE TupleSections #-}
module Test where
    

import System.Random
import Control.Monad.State

type TestValue = [Int]
type TestState = (StdGen, [TestValue])

initState seed = (mkStdGen seed, [])

range = (1, 10)

getVal :: State TestState Int
getVal = do
  (gen, _) <- get
  let (val, gen') = randomR range gen
  modify ((gen',) . snd)
  return val

getNext :: State TestState TestValue
getNext = do
  val <- getVal
  vs <- snd <$> get
  if [val] `elem` vs
  then getNext
  else do modify ((, [val] : vs) . fst)
          return [val]


test :: State TestState TestValue
test = do
   getNext
   getNext
   getNext
   return []


{-

expect = do
  freshName <- getNextFreshName
  ...
-}
