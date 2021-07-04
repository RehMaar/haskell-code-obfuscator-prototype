module Main where

data R = R { r1 :: Int, r2 :: Double }

-- wildcards
pat1 _ = 1
-- VarPat
pat2 x = 1
-- AsPat
pat3 x@Nothing = 1
pat3 x@(Just y) = 1
-- ParPat
pat4 (x) = 1
-- ListPat
pat5 [x] = 1
-- TuplePat
pat6 (x, y) = 1
-- NPat
pat7 1 = 1
pat8 1.2 = 1
-- LitPat
pat9 "a" = 1
pat10 :: Int -> Int
pat10 1 = 1
-- ConPatIn
pat11 Nothing = 1
pat11 (Just x) = 2
-- LazyPat
pat12 ~(a, b) = 1

-- Records
patRecords R { r1 = x, r2 = y } = 1

main = undefined
