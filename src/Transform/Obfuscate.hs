{-# LANGUAGE TypeFamilies, FlexibleInstances, OverloadedStrings, RankNTypes #-}

module Transform.Obfuscate where

import           Language.Haskell.GHC.ExactPrint
                                               as EP hiding (Transform)
import           Language.Haskell.GHC.ExactPrint.Parsers
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Utils
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Delta
                                               as EP

import           GHC.SourceGen                 as SG
import           GHC.SourceGen.Binds           as SG

import           GHC
import qualified OccName                       as GHC
import qualified RdrName                       as GHC
import qualified SrcLoc                        as GHC
import qualified Bag                           as GHC
import qualified Name                          as GHC
import qualified Module                        as GHC
import qualified FastString                    as GHC
import qualified BasicTypes                    as GHC
import qualified HscTypes                      as GHC
import qualified TcRnTypes                     as GHC
import qualified UniqDFM                       as GHC
import           GHC.Paths                      ( libdir )
import           DynFlags
import           TcEvidence                     ( HsWrapper(WpHole) )
import qualified Outputable                    as Out


import           Data.Generics                 as SYB
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import           Data.String
import           Data.Char
import           Control.Arrow                  ( (&&&), first )
import           System.Random
import Control.Monad
import Control.Monad.State
import Data.Foldable

import           OneLinePrinter
import           Utils
import           Source
import           Transform.Internal.Types
import           Transform.Internal.Query
import           Transform.Internal.Context
import           Transform.Rename
import           Transform.Desugar
import           Transform.Literal
import           Transform.Function

import           Debug.Trace
import System.Directory (makeAbsolute)

data ApplyTrans
  = ApplyTrans
    { changeNames :: ApplyRenaming
    , changeStructure :: Bool
    , changeStrings :: Bool
    }
  deriving Show

data ApplyRenaming
  = RenameAll -- ^ Rename local symbols and imports
  | RenameLocal
  | RenameNone
  deriving Show

defaultApplyTrans :: ApplyTrans
defaultApplyTrans = ApplyTrans RenameNone True True

generateRenamings :: Transform [(String, String)]
generateRenamings = do
  sc <- gets tc_source_ctx
  let globals = sc_allow_rename_globals sc
  let locals  = varname . lcelem <$> sc_allow_rename_locals sc
  let rvs = unique $ sort $ globals ++ locals
  generateRenamings' rvs
  where
    generateRenamings' :: [String] -> Transform [(String, String)]
    generateRenamings' [] = return []
    generateRenamings' (n:ns) = do
      name <- getNextFreshVar
      rs   <- generateRenamings' ns
      return ((n, name):rs)

obfuscateNames :: Bool -> Transform ()
obfuscateNames renameImports = do
  ctx <- get
  renamings <- generateRenamings
  let src = tc_parsed_source ctx
  let src1 = rename (tc_source_ctx ctx) renamings src
  let src2 = renameImportedSymbols (tc_source_ctx ctx) renamings src1
  setSource src2

obfuscateStrings :: Transform ()
obfuscateStrings = do
  transformStringAndChars

obfuscateStructure :: Transform ()
obfuscateStructure = do
  transformDoToLam
  applyTransformation addParens
  applyTransformationCommon applyTopDown transformOpToApp
  applyTransformation transformIfCase
  applyTransformation transformMultiArgLam

obfuscate = obfuscateWithSeed defaultApplyTrans 0

obfuscateWithSeed :: ApplyTrans -> Int -> SourceInfo -> ParsedSource
obfuscateWithSeed ApplyTrans{..} = evalTransform obfuscate''
  where
    obfuscate'' = do
      case changeNames of
        RenameNone -> pure ()
        RenameLocal -> obfuscateNames False
        RenameAll -> obfuscateNames True
      when changeStrings $
        obfuscateStrings
      when changeStructure $
        obfuscateStructure
      gets tc_parsed_source

obfuscateFileInProj
  :: FilePath -> Maybe Int -> FilePath -> ApplyTrans -> IO ()
obfuscateFileInProj args = obfuscateCommon (ProjectModule args)

obfuscateFile
  :: [String] -> Maybe Int -> FilePath -> ApplyTrans -> IO ()
obfuscateFile args = obfuscateCommon (SimpleModule args)

obfuscateCommon :: ModuleType -> Maybe Int -> FilePath -> ApplyTrans -> IO ()
obfuscateCommon mod seed path flags = do
  absPath <- makeAbsolute path
  si <- handleModule mod absPath
  seed <- maybe randomIO return seed
  let src = obfuscateWithSeed flags seed si
  let dflags = si_dynflags si
  -- let code = Out.showSDocOneLine dflags $ oneline (si_annotations si) $ unLoc src
  let code = Out.showSDoc dflags $ Out.ppr src
  putStrLn code
