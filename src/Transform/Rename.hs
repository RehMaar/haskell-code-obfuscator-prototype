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

import qualified Outputable as Out

import OneLinePrinter
import Utils
import Source
import Transform.Types
import Transform.Query
import Transform.Context


lookupRenaming [] _ = Nothing
lookupRenaming ((oldname, newname):rn) name
  | oldname == GHC.occNameString (GHC.rdrNameOcc $ unLoc name)
  = Just newname
  | otherwise
  = lookupRenaming rn name

newRdrName name (GHC.Unqual _)     = GHC.Unqual (GHC.mkVarOcc name)
newRdrName name (GHC.Qual mod occ) = GHC.Qual mod (GHC.mkVarOcc name)
newRdrName name (GHC.Orig mod occ) = GHC.Orig mod (GHC.mkVarOcc name)
newRdrName _ _ = error "newRdrName: Exact ctr. How is it used?"

lookupVar :: [Loc Var] -> Loc String -> Maybe (Loc Var)
lookupVar = lookupGen (\var name -> (lcloc name == lcloc var) &&
                                    (varname (lcelem var) == lcelem name))id
lookupDef = lookupGen (\def name -> name == tldefname def) id

rename :: TransformContext -> [(String, String)] -> Anns -> ParsedSource -> (Anns, ParsedSource)
rename octx renamings ans mod = do
  (ans,) $
    apply changerBind $
    apply changerMatch $
    apply changerPat $
    apply changerVar mod
  where
   changerBind :: GHC.HsBind GhcPs -> GHC.HsBind GhcPs
   changerBind (FunBind a name b c d)
     | Just newName <- getNewNameDef octx name
     = FunBind a (newRdrName newName <$> name) b c d
   changerBind  x = x

   changerMatch :: HsMatchContext RdrName -> HsMatchContext RdrName
   changerMatch (FunRhs name a b)
     | Just newName <- getNewNameDef octx name
     = FunRhs (newRdrName newName <$> name) a b
     -- = FunRhs (noLoc $ newRdrName newName $ unLoc name) a b
   changerMatch x = x

   -- TODO: also change name of type signature
   -- changerSig

   changerPat :: GHC.Pat GhcPs -> GHC.Pat GhcPs
   changerPat (VarPat a name)
     | Just newName <- lookupRenaming renamings name
     = VarPat a (newRdrName newName <$> name)
     -- = VarPat a (noLoc $ newRdrName newName $ unLoc name)
   changerPat x = x

   changerVar :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
   changerVar (HsVar ext name)
     | Just newName <- getNewNameVar octx name
     = HsVar ext (newRdrName newName <$> name)
     -- = HsVar ext (noLoc $ newRdrName newName $ unLoc name)
   changerVar x = x

   -- getRenaming octx = lookupRenaming (tcRenamings octx)

   getNewNameVar octx name
     | Loc loc (Var name' qual') <- rdrnameToVar name
     , Just (Loc _ (Var _ qual)) <- lookupVar (tcVars octx) (Loc loc name')
     , fromMaybe "" qual == tcModName octx && (name' `notElem` tcExported octx) || isNothing qual
     , Just newName <- lookupRenaming renamings name
     = Just newName
     | otherwise = Nothing

   getNewNameDef octx name
     | loc    <- getLoc name
     , name'  <- rdrName2String $ unLoc name
     , Just _ <- find (== Loc loc name') (tcTopLevelDefs octx)
     , name' `notElem` tcExported octx
     = lookup name' renamings
     | loc     <- getLoc name
     , name'   <- rdrName2String $ unLoc name
     , Nothing <- find (== Loc loc name') (tcTopLevelDefs octx)
     = lookup name' renamings
     | otherwise = Nothing

-- renameImportedSymbols :: TransformContext -> Anns -> Located (HsModule GhcPs) -> IO (Anns, Located (HsModule GhcPs))
{-renameImportedSymbols octx generator ans src = do
    src <- return $ apply changerImported src
    let decls = uncurry newDecl <$> importedRenamings
    let src' = foldl (\s decl -> addDecl decl <$> s) src decls
    return (ans, src')
  where
    importedSymbols = filter ((\q -> isJust q && fromJust q /= tcModName octx) . varqual . lcelem) $ tcVars octx
    importedRenamings = zip importedSymbols $
                            generator (succ $ length $ tcNames octx) $ fmap (varname . lcelem) importedSymbols

    newDecl (Loc _ var) newName = createDecl newName $ varname var

    changerImported :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
    changerImported (HsVar ext name)
      | Just newName <- getNewName name
      = HsVar ext (newRdrName newName <$> name)
      -- = HsVar ext (noLoc $ newRdrName newName $ unLoc name)
    changerImported x = x

    getNewName name = lookupIR (rdrnameToVar name) importedRenamings

    lookupIR _ [] = Nothing
    lookupIR n@(Loc loc var) ((Loc loc' var', newName):irs)
      | loc == loc'
      , varname var == varname var'
      = Just newName
      | otherwise = lookupIR n irs-}

renameImportedSymbols :: TransformContext -> [(String, String)] -> Anns -> ParsedSource -> (Anns, ParsedSource)
renameImportedSymbols octx renamings ans src = let
    src' = apply changerImported src
    decls = uncurry newDecl <$> importedRenamings
    src'' = foldl (\s decl -> addDecl decl <$> s) src' decls
    in (ans, src'')
  where
    importedSymbols :: [Loc Var]
    importedSymbols = filter ((\q -> isJust q && fromJust q /= tcModName octx) . varqual . lcelem) $ tcVars octx

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
