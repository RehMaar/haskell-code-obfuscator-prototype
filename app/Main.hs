{-# LANGUAGE TypeFamilies, DeriveDataTypeable, RankNTypes #-}
module Main where
    
import Language.Haskell.GHC.ExactPrint.Parsers

import qualified Outputable as O
import SrcLoc
import qualified Util

import Control.Arrow
import System.Environment

import Data.Maybe
import System.Random

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
  , seed  :: Maybe Int
  , flags :: ObfTypeFlags
  }
  deriving Show

programInfo :: ParserInfo ObfArgs
programInfo = info (obfArgs <**> helper) desc
  where
    obfArgs = ObfArgs <$> arg <*> getSeed <*> flags
    getSeed = (Just <$> getSeed') <|> pure Nothing
    getSeed' = option auto
              ( long "seed"
             <> help "Seed for random generation"
             <> metavar "INT")


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
    handleFlags (ObfArgs file seed (ProjectModuleFlags wdir)) = obfuscateFileInProj wdir seed file
    handleFlags (ObfArgs file seed (SimpleModuleFlags opts)) = do
        case Util.toArgs opts of
          Right args -> obfuscateFile args seed file
          Left _ -> putStrLnErr "error: Unable to parse ghc options"
    handleFlags (ObfArgs file seed NoOpts) = obfuscateFile [] seed file

obfuscateFileInProj arg seed file = obfuscateCommon (ProjectModule arg) seed file
obfuscateFile       arg seed file = obfuscateCommon (SimpleModule arg) seed file

obfuscateCommon mod seed path = do
  absPath <- makeAbsolute path
  si <- handleModule mod absPath
  seed <- maybe randomIO return seed
  let src = obfuscateWithSeed seed si
  let dflags = si_dynflags si
  let code = O.showSDocOneLine dflags $ oneline (si_annotations si) $ unLoc src
  -- let code = O.showSDoc dflags $ O.ppr src
  putStrLn code
