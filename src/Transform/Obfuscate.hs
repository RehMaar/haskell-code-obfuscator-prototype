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

obfuscateNames :: Transform ParsedSource
obfuscateNames = do
  ctx <- get
  renamings <- generateRenamings
  let src1 = rename (tc_source_ctx ctx) renamings (tc_parsed_source ctx)
  -- let src2 = renameImportedSymbols (tc_source_ctx ctx) renamings src1
  setSource src1
  return src1

obfuscateStructure :: Transform ParsedSource
obfuscateStructure = do
  -- transformStringAndChars
  transformDoToLam
  -- applyTransformation addParens
  -- applyTransformationCommon applyTopDown transformOpToApp
  -- applyTransformation transformIfCase
  -- applyTransformation transformMultiArgLam
  gets tc_parsed_source

obfuscate = obfuscateWithSeed 0

obfuscateWithSeed :: Int -> SourceInfo -> ParsedSource
obfuscateWithSeed = evalTransform obfuscate''
  where
    obfuscate'' = do
      obfuscateNames
      obfuscateStructure

obfuscateFileInProj arg seed file = obfuscateCommon (ProjectModule arg) seed file
obfuscateFile       arg seed file = obfuscateCommon (SimpleModule arg) seed file

obfuscateCommon mod seed path = do
  absPath <- makeAbsolute path
  si <- handleModule mod absPath
  seed <- maybe randomIO return seed
  let src = obfuscateWithSeed seed si
  let dflags = si_dynflags si
  let code = Out.showSDocOneLine dflags $ oneline (si_annotations si) $ unLoc src
  -- let code = O.showSDoc dflags $ O.ppr src
  putStrLn code
