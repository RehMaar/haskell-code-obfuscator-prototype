{-# LANGUAGE TypeFamilies, TupleSections #-}
module Transform.Rename (rename, renameImportedSymbols) where

import Language.Haskell.GHC.ExactPrint as EP
import Language.Haskell.GHC.ExactPrint.Parsers as EP
import Language.Haskell.GHC.ExactPrint.Utils as EP
import Language.Haskell.GHC.ExactPrint.Delta as EP

import qualified GHC.SourceGen as SG
import qualified GHC.SourceGen.Binds as SG

import Data.Generics as SYB

import GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified Bag as GHC
import qualified Name as GHC
import qualified Module as GHC
import qualified FastString as GHC
import qualified BasicTypes as GHC
import qualified HscTypes as GHC
import qualified TcRnTypes as GHC
import qualified UniqDFM as GHC
import GHC.Paths (libdir)
import DynFlags

import TcEvidence (HsWrapper(WpHole))

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.String

import Control.Arrow ((&&&))
import Control.Applicative ((<|>))

import qualified Outputable as Out

import OneLinePrinter
import Utils
import Source
import Transform.Types
import Transform.Query
import Transform.Context

import Debug.Trace

lookupRenaming [] _ = Nothing
lookupRenaming ((oldname, newname):rn) name
  | oldname == GHC.occNameString (GHC.rdrNameOcc $ unLoc name)
  = Just newname
  | otherwise
  = lookupRenaming rn name

newRdrName name (GHC.Unqual _)     = GHC.Unqual (GHC.mkVarOcc name)
newRdrName name (GHC.Qual mod occ) = GHC.Qual mod (GHC.mkVarOcc name)
newRdrName name (GHC.Orig mod occ) = GHC.Orig mod (GHC.mkVarOcc name)
newRdrName name (GHC.Exact name')
  | Just mod <- GHC.nameModule_maybe name'
  = GHC.Orig mod (GHC.mkVarOcc name)
  | otherwise
  = GHC.Unqual (GHC.mkVarOcc name)

lookupVar :: [Loc Var] -> Loc String -> Maybe (Loc Var)
lookupVar = lookupGen (\var name -> (lcloc name == lcloc var) &&
                                    (varname (lcelem var) == lcelem name))id
lookupDef = lookupGen (\def name -> name == tldefname def) id

rename :: SourceContext -> [(String, String)] -> ParsedSource -> ParsedSource
rename sc renamings mod =
    -- Change fields in data types and records.
    apply changerRecords$
    -- Change type signatures
    apply changerSig $
    -- Change function definitions
    apply changerBind $
    apply changerMatch $
    -- Change arguments
    apply changerPat $
    -- Change variables
    apply changerVar mod
  where

   changerRecords :: GHC.FieldOcc GhcPs -> GHC.FieldOcc GhcPs
   changerRecords (FieldOcc _ name)
     | Just newName <- getNewNameDef sc name
     = FieldOcc noExt (newRdrName newName <$> name)
   changerRecords x = x

   changerBind :: GHC.HsBind GhcPs -> GHC.HsBind GhcPs
   changerBind (FunBind a name b c d)
     | Just newName <- getNewNameDef sc name
     = FunBind a (newRdrName newName <$> name) b c d
   changerBind  x = x

   changerMatch :: HsMatchContext RdrName -> HsMatchContext RdrName
   changerMatch (FunRhs name a b)
     | Just newName <- getNewNameDef sc name
     = FunRhs (newRdrName newName <$> name) a b
   changerMatch x = x

   changerSig :: GHC.Sig GhcPs -> GHC.Sig GhcPs
   changerSig (TypeSig _ names typ)
     | Just names' <- sequence $ map getSigName names
     = TypeSig noExt names' typ
   changerSig (ClassOpSig _ flag names typ)
     | Just names' <- sequence $ map getSigName names
     = ClassOpSig noExt flag names' typ
   changerSig x = x

   getSigName :: Located RdrName -> Maybe (Located RdrName)
   getSigName name
     | Just newName <- lookupRenaming renamings name
     = Just (newRdrName newName <$> name)
     | otherwise
     = Nothing

   changerPat :: GHC.Pat GhcPs -> GHC.Pat GhcPs
   changerPat (VarPat a name)
     | Just newName <- lookupRenaming renamings name
     = VarPat a (newRdrName newName <$> name)
   changerPat x = x

   changerVar :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
   changerVar (HsVar ext name)
     | Just newName <- getNewNameVar sc name
     = HsVar ext (newRdrName newName <$> name)
   changerVar x = x

   -- Check if we need to rename <name> and return a renaming.
   -- Name is allowed to rename if it is in the `sc_allow_rename_locals` list.
   getNewNameVar sc name
     | Loc loc (Var name' _) <- rdrnameToVar name
     , Just (Loc _ (Var _ qual)) <- lookupVar (sc_allow_rename_locals sc) (Loc loc name')
     , Just newName <- lookupRenaming renamings name
     = Just newName
     | otherwise = Nothing

   -- TODO: test it better!
   getNewNameDef :: SourceContext -> Located RdrName -> Maybe String
   getNewNameDef sc name
     | l@(Loc loc (Var name' _)) <- rdrnameToVar name
     , Just _ <- find (== name') (sc_allow_rename_globals sc ++
                    ((varname . lcelem) <$> sc_allow_rename_locals  sc))
     = lookup name' renamings
     | otherwise = Nothing

renameImportedSymbols :: SourceContext -> [(String, String)] -> ParsedSource -> ParsedSource
renameImportedSymbols sc renamings src = let
    src' = apply changerImported src
    decls = uncurry newDecl <$> importedRenamings
    src'' = foldl (\s decl -> addDecl decl <$> s) src' decls
    in src''
  where
    importedSymbols :: [Loc Var]
    importedSymbols = filter ((\q -> isVarQualified q && getVarQual q /= sc_module_name sc) . lcelem) $ sc_allow_rename_locals sc

    importedRenamings :: [(Loc Var, String)]
    importedRenamings = catMaybes $ map findImported renamings

    findImported (oldName, newName)
      | Just var <- find (\(Loc _ n) -> varname n == oldName) importedSymbols
      = Just (var, newName)
    findImported _ = Nothing

    newDecl (Loc _ var) newName = createDecl newName $ varname var

    changerImported :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
    changerImported (HsVar ext name)
      | Just newName <- getNewName name
      = HsVar ext (newRdrName newName <$> name)
    changerImported x = x

    getNewName name = lookupIR (rdrnameToVar name) importedRenamings

    lookupIR _ [] = Nothing
    lookupIR n@(Loc loc var) ((Loc loc' var', newName):irs)
      | loc == loc'
      , varname var == varname var'
      = Just newName
      | otherwise = lookupIR n irs
