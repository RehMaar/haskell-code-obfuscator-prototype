module Main where

import Language.Haskell.GHC.ExactPrint.Parsers

import Outputable
import SrcLoc

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
  writeFile "resources/o.hs" code
