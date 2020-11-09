{-# LANGUAGE TypeFamilies, DeriveDataTypeable, RankNTypes #-}
module Main where
    
import Language.Haskell.GHC.ExactPrint.Parsers

import qualified Outputable as O
import SrcLoc
import qualified Util

import Control.Arrow
import System.Environment

import Transform.Obfuscate
import Source
import OneLinePrinter
import Utils

import System.Directory (makeAbsolute)
import Options.Applicative

data ObfTypeFlags
  = SimpleModuleFlags { ghcOpts :: String }
  | ProjectModuleFlags { projectWdir :: String }
  | NoOpts
  deriving Show

data ObfArgs
  = ObfArgs
  { file :: FilePath
  , flags :: ObfTypeFlags
  }
  deriving Show

programInfo :: ParserInfo ObfArgs
programInfo = info (obfArgs <**> helper) desc
  where
    obfArgs = ObfArgs <$> arg <*> flags
    arg = argument str (metavar "FILE")
    flags = simpleModuleFlags <|> projectModuleFlags <|> noOpts
    noOpts = pure NoOpts
    simpleModuleFlags = SimpleModuleFlags
      <$> strOption
          (long "ghc-options"
           <> metavar "GHC_OPTIONS"
           <> help "GHC options to compile the given module")
    projectModuleFlags = ProjectModuleFlags
      <$> strOption
          (long "working-dir"
          <> short 'w'
          <> metavar "WDIR"
          <> help "Path to a project directory")

    desc = fullDesc
         <> progDesc "Description"
         <> header "obfuscate -- an utility for haskell code obfuscation"


main = do
  flags <- execParser programInfo
  handleFlags flags
  where
    handleFlags (ObfArgs file (ProjectModuleFlags wdir)) = obfuscateFileInProj wdir file
    handleFlags (ObfArgs file (SimpleModuleFlags opts)) = do
        case Util.toArgs opts of
          Right args -> obfuscateFile args  file
          Left _ -> putStrLnErr "error: Unable to parse ghc options"
    handleFlags (ObfArgs file NoOpts) = obfuscateFile [] file

obfuscateFileInProj = obfuscateCommon . ProjectModule

obfuscateFile = obfuscateCommon . SimpleModule

obfuscateCommon mod path = do
  absPath <- makeAbsolute path
  si <- handleModule mod absPath
  let src = obfuscate si
  let dflags = si_dynflags si
  let code = O.showSDocOneLine dflags $ oneline (si_annotations si) $ unLoc src
  -- let code = O.showSDoc dflags $ O.ppr src
  putStrLn code

{--- For debug
obfuscateS path = do
  si <- handleModule (SimpleModule []) path
  let (_, src) = obfuscateStructure si
  let dflags = si_dynflags si
  --let code = showSDocOneLine dflags $ oneline $ unLoc src
  let code = O.showSDocUnsafe $ O.ppr src
  putStrLn code-}
