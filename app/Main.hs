module Main where

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
  -- let code = showSDocOneLine dflags $ oneline $ unLoc src
  let code = showSDocUnsafe $ ppr src
  putStrLn code
