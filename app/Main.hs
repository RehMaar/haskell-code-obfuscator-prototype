module Main where

import Language.Haskell.GHC.ExactPrint.Parsers

import Outputable
import SrcLoc

import Control.Arrow

import Transform.Obfuscate
import Source
import OneLinePrinter

main = putStrLn "Privet"

obfuscateFileInProj = obfuscateCommon . ProjectModule


obfuscateFile = obfuscateCommon SimpleModule

obfuscateCommon mod path = do
  si <- handleModule mod path
  let (_, src) = obfuscate si
  let dflags = si_dynflags si
  let code = showSDocOneLine dflags $ oneline $ unLoc src
  --let code = showSDocUnsafe $ ppr $ si_parsed_source si
  putStrLn code
  -- Right (ans, src) <- parseModuleFromString path code
  -- putStrLn $ showSDocDebug dflags $ ppr src
  -- let newPath = newFileName path
  -- if newPath == path
  -- then fail "Unable to generate unique filename"
  -- else writeFile newPath code

-- better not to use on filenames without '.hs'
newFileName path = let
   (extR, nameR) = break' (== '.') $ reverse path
   name = reverse nameR
   ext = reverse extR
  in name ++ "_obf" ++ ext

break' _ [] = ([], [])
break' cond (x:xs) | cond x = ([x], xs)
                   | otherwise = first (x:) $ break' cond xs
