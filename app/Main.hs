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

data ObfFlagsModule
  = SimpleModuleFlags { ghcOpts :: String }
  | ProjectModuleFlags { projectWdir :: String }
  | NoModuleOpts
  deriving Show

data ObfArgs
  = ObfArgs
  { file :: FilePath
  , seed  :: Maybe Int
  , moduleFlags :: ObfFlagsModule
  , flags :: ApplyTrans
  }
  deriving Show

programInfo :: ParserInfo ObfArgs
programInfo = info (obfArgs <**> helper) desc
  where
    obfArgs =
     ObfArgs
       <$> arg
       <*> getSeed
       <*> moduleFlags
       <*> transformFlags

    arg = argument str (metavar "FILE")
    getSeed = (Just <$> getSeed') <|> pure Nothing
    getSeed' = option auto
              ( long "seed"
             <> help "Seed for random generation"
             <> metavar "INT")

    moduleFlags = simpleModuleFlags <|> projectModuleFlags <|> noOpts
      where
        noOpts = pure NoModuleOpts
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
    transformFlags = allTrans <|> someTrans
      where
        allTrans = flag' defaultApplyTrans (long "all" <> help "Apply all transformations")
        someTrans = ApplyTrans
          <$> doRename
          <*> doBasicStructure
          <*> doStrings
          where
            doRename =
              flag RenameNone RenameLocal (long "rename" <> help "Rename local variables")
                <|> flag RenameNone RenameAll (long "rename-all" <> help "Rename local variables and imported symbols")
            doBasicStructure =
              flag True False(long "no-basic" <> help "Do not apply basic structural transformations")
            doStrings =
              flag False True (long "strings" <> help "Apply string transformation")

    desc = fullDesc
         <> progDesc "Description"
         <> header "obfuscate -- an utility for haskell code obfuscation"


main = do
  flags <- execParser programInfo
  handleFlags flags
  where
    handleFlags (ObfArgs file seed (ProjectModuleFlags wdir) flags) = obfuscateFileInProj wdir seed file flags
    handleFlags (ObfArgs file seed (SimpleModuleFlags opts) flags) = do
        case Util.toArgs opts of
          Right args -> obfuscateFile args seed file flags
          Left _ -> putStrLnErr "error: Unable to parse ghc options"
    handleFlags (ObfArgs file seed NoModuleOpts flags) = obfuscateFile [] seed file flags
