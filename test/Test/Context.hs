module Test.Context where

import SrcLoc
import Data.String
import Test.HUnit

import Data.List

import Source
import Transform.Context
import Transform.Types

uncurry3 f (a, b, c) = f a b c

createLoc name line sCol eCol = Loc $ mkSrcSpan sLoc eLoc
  where
    sLoc = mkSrcLoc (fromString name) line sCol
    eLoc = mkSrcLoc (fromString name) line eCol

-- TODO: how people do it?
testRoot = {- projectDir/ -} "test/resources/"

testSourceContextFields file selector correct = TestCase $ do
  si <- handleModule (SimpleModule []) file
  let sc = initSourceContext si
  assertEqual "" (sort correct) (sort $ selector sc)

-- Check if list of names that are allowed to be renamed is as excepted
testLocals1 = (fileName, sc_allow_rename_locals, localNames)
  where
    fileName = testRoot ++ "locals.hs"
    createLoc' = createLoc fileName
                  -- test2
    localNames = [ createLoc' 8  7  8  $ mkVarNoQual "x"
                 , createLoc' 8  11 12 $ mkVarNoQual "x"
                  -- test3
                 , createLoc' 11 7  8  $ mkVarNoQual "x"
                 , createLoc' 11 9  10 $ mkVarNoQual "y"
                 , createLoc' 11 33 34 $ mkVarNoQual "z"
                 , createLoc' 11 37 38 $ mkVarNoQual "y"
                 , createLoc' 11 41 42 $ mkVarNoQual "p"
                 , createLoc' 11 45 46 $ mkVarNoQual "y"
                   -- test 4
                 , createLoc' 14 7  8  $ mkVarNoQual "x"
                 , createLoc' 14 17 18 $ mkVarNoQual "x"
                   -- test5
                 , createLoc' 17 7  8  $ mkVarNoQual "x"
                 , createLoc' 17 19 20 $ mkVarNoQual "f"
                 , createLoc' 17 21 22 $ mkVarNoQual "x"
                   -- test5:f
                 , createLoc' 18 11 12 $ mkVarNoQual "x"
                 , createLoc' 18 33 34 $ mkVarNoQual "x"
                 , createLoc' 18 37 42 $ mkVarNoQual "test3"
                 , createLoc' 18 58 59 $ mkVarNoQual "x"
                 ]

testGlobals1 = (fileName, sc_allow_rename_globals, globalNames)
  where
    fileName = testRoot ++ "exports.hs"
    globalNames = ["test1", "test3", "ctr", "b_a", "b_b", "f'", "f''", "hmap", "jjmap" ]

tests
  = TestList
  [ TestLabel "Allow to rename locals" $ uncurry3 testSourceContextFields testLocals1
  , TestLabel "Allow to rename globals" $ uncurry3 testSourceContextFields testGlobals1
  ]
