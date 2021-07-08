{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Transform.Internal.Context
       where

import           Language.Haskell.GHC.ExactPrint.Utils
                                               as EP

import           GHC
import qualified Avail                         as GHC
import qualified FieldLabel                    as GHC
import qualified OccName                       as GHC
import qualified RdrName                       as GHC
import qualified SrcLoc                        as GHC
import qualified Bag                           as GHC
import qualified Name                          as GHC
import qualified Module                        as GHC
import qualified FastString                    as GHC
import qualified BasicTypes                    as GHC

import           Data.Maybe
import           Data.List
import           Control.Arrow                  ( first )

import           Transform.Internal.Types
import           Transform.Internal.Query
import           Source
import           Utils
import Data.Typeable
import Data.Data
import Control.Monad.State
import System.Random
import Data.Char
import Debug.Trace
import qualified GHC.SourceGen as SG


data TransformContext
  = TC
  { tc_source_ctx:: SourceContext
  , tc_parsed_source :: ParsedSource
  , tc_symbols :: [Char]
  , tc_range_symbols :: (Int, Int)
  , tc_range_name_len :: (Int, Int)
  , tc_generator :: StdGen
  , tc_used_symbols :: [String]
  , tc_new_decls :: [SG.HsDecl']
  }

type Transform a = State TransformContext a

evalTransform :: State TransformContext ParsedSource -> Int -> Source.SourceInfo -> ParsedSource
evalTransform f seed = evalState (updateDecls =<< f) . initTransform seed

updateDecls :: ParsedSource -> Transform ParsedSource
updateDecls src = do
  ds <- gets tc_new_decls
  pure $ update ds <$> src
  where
    update ds (HsModule n e i ds' x y) = HsModule n e i (ds' ++ fmap noLoc ds) x y

-- TODO: fill used symbols with already used in source code
initTransformCommon symbols range seed si = let
    sctx = initSourceContext si
  in TC sctx (si_parsed_source si) symbols (0, pred $ length symbols) range (mkStdGen seed) [] []

initTransform = initTransformCommon defaultSymbols defaultRange
  where
    defaultSymbols = ['A'..'Z'] ++ ['a'..'z']
    defaultRange = (1, 3)

addNewDecl :: SG.HsDecl' -> Transform ()
addNewDecl decl =
  modify (\ctx -> ctx { tc_new_decls = decl : tc_new_decls ctx })

setSource :: ParsedSource -> Transform ()
setSource src = do
  modify (\ctx -> ctx { tc_parsed_source = src })

getSource :: Transform ParsedSource
getSource = gets tc_parsed_source

isUsed :: String -> Transform Bool
isUsed name = do
  used <- gets tc_used_symbols
  return $ name `elem` used

markUsed :: String -> Transform ()
markUsed name =
  modify (\ctx -> ctx { tc_used_symbols = name : tc_used_symbols ctx })

getNextFreshVar = getNextFreshNameShort

getNextFreshNameShort :: Transform String
getNextFreshNameShort = do
  used <- gets tc_used_symbols
  let sorted = sortOn length used
  symbols <- gets tc_symbols
  let name = findShortest sorted symbols 1
  markUsed name
  pure name
  where
    findShortest used symbols n
      | Just name <- findWithLen used symbols n
      = name
      | otherwise
      = findShortest used symbols (succ n)

    findWithLen used symbols n =
      find (not . (`elem` used)) $
        toLowerCase <$>
          replicateM n symbols
    toLowerCase (x:xs) = toLower x : xs

-- TODO: generate fresh names based on the context,
-- local variables in to different functions can have the same name
getNextFreshVar' :: Transform String
getNextFreshVar' = do
  name <- getNextFreshName
  case name of
    []     -> error "getNextFreshVar: empty name"
    (n:ns) -> do
      let name' = (toLower n : ns)
      flag <- isUsed name'
      if flag
      then getNextFreshVar
      else do
        markUsed name
        return name'

getNextFreshName :: Transform String
getNextFreshName = do
   wordLen <- getWordLen
   name <- getName wordLen
   flag <- isUsed name
   if flag
   then getNextFreshName
   else do
     markUsed name
     return name
  where
    getName :: Int -> Transform String
    getName 0 = return ""
    getName wordLen = do
      symb <- getSymbol
      rest <- getName (pred wordLen)
      return (symb:rest)

    getSymbol = do
      ctx <- get
      idx <- getInt (tc_range_symbols ctx)
      return $ tc_symbols ctx !! idx

    getWordLen :: Transform Int
    getWordLen = do
      range <- gets tc_range_name_len
      getInt range

    getInt :: (Int, Int) -> Transform Int
    getInt range = do
      gen <- gets tc_generator
      let (int, gen') = randomR range gen
      modify (\ctx -> ctx { tc_generator = gen' })
      return int

applyTransformation :: Typeable a => (a -> a) -> Transform ()
applyTransformation = applyTransformationCommon apply

applyTransformationCommon
  :: (Typeable a)
  => (forall a b . (Data b, Typeable a) => (a -> a) -> b -> b)
  -> (a -> a)
  -> Transform ()
applyTransformationCommon applier f = do
  src <- gets tc_parsed_source
  modify (\ctx -> ctx { tc_parsed_source = applier f src })

data SourceContext
  = SC
  { sc_module_name :: String
  , sc_decls :: [Decl]
  , sc_exported :: Maybe [Exported]
   -- ^ List of exported symbols.
   -- Possible states:
   --  * Nothing -- everything is exported
   --  * Just [] -- nothing is exported
   --  * Just xs -- export only symbols in the list
  , sc_allow_rename_locals :: [Loc Var]
  -- ^ Local variables that are defined in the source code
  , sc_allow_rename_globals :: [String]
  -- ^ Top level names that are defined in the source code
  , sc_imported_symbols :: [Loc Var]
  }
  deriving Show

data Exported
  = ExportedTyCl { e_typeName :: Var, e_ctrs :: [Var], e_fields :: [Var] }
  | ExportedFun  { e_funName :: Var }
  deriving Show

data Con = Con { cn_name :: String, cn_fields :: [String] }
  deriving Show

data Decl
 = DataD      { dt_name :: String, dt_ctrs :: [Con] }
 | ClassD     { cls_name :: String, cls_funs :: [String] }
 | FunD       { fun_name :: Loc String
              , fun_args :: [Loc String]
              , fun_inner_decls :: [Decl]
              , fun_vars :: [Loc Var]}
 | InstanceD  { inst_funs :: [Decl]
              -- ^ Only FunD decls are expected.
              -- Maybe, need it later.
              -- , inst_className :: String
              -- , inst_typeName :: String
              }
 -- Probably, need it later
 -- | TypeDecl  { tp_name :: Loc Def  }
 -- | SigDecl   { decl_name :: Loc Def}
  deriving Show

isInst InstanceD {} = True
isInst _ = False

initSourceContext :: SourceInfo -> SourceContext
initSourceContext (SourceInfo _ src rvs exports _) =
  let modName   = fromMaybe "Main" $ getModuleName $ unLoc src
      decls     = collectDecls $ unLoc src
      exported  = collectExportedSym <$> exports
      decls'    = addQualifications' (namesToVars rvs) decls
      (allow_rename_globals, allow_rename_locals, imported_symbols) =
          collectAllowRename modName decls' exported
  in  SC { sc_module_name          = modName
         , sc_decls                = decls'
         , sc_exported             = exported
         , sc_allow_rename_locals  = allow_rename_locals
         , sc_allow_rename_globals = allow_rename_globals
         , sc_imported_symbols     = imported_symbols
         }

-- Add qual only to vars (inner decls aren't touched)
addQualifications' :: [Loc Var] -> [Decl] -> [Decl]
addQualifications' = map . addQual
 where
  addQual :: [Loc Var] -> Decl -> Decl
  addQual rvs f@FunD { fun_vars = vars } =
    f { fun_vars = map (changeQual rvs) vars }
  addQual _ d = d

  changeQual :: [Loc Var] -> Loc Var -> Loc Var
  changeQual rvs (Loc loc (Var v qual))
    | (x : _) <- filter (\l -> lcloc l == loc && getVarName (lcelem l) == v) rvs
    = Loc loc $ mkVar v $ newQual qual $ varqual $ lcelem x
  changeQual _ x = x

  newQual q@(PQual _) _      = q
  newQual _           NoQual = NoQual
  newQual _           q      = RQual $ getRealQual q

-- collectAllowRename
--   :: String -> [Decl] -> Maybe [Exported] -> ([String], [Loc Var])
collectAllowRename modName decls exported =
  let topLevelToRename = allowRenameTopLevel modName decls exported
      (localsToRename, imported) = allowRenameLocals modName decls topLevelToRename
  in (topLevelToRename, localsToRename, imported)

funLocals FunD { fun_args = args, fun_vars = vars, fun_inner_decls = ds} =
  (fmap mkVarNoQual <$> args) ++ vars ++ concatMap funLocalsArgs ds
funLocals _ = []
funLocalsArgs FunD { fun_args = args, fun_inner_decls = ds } = (fmap mkVarNoQual <$> args) ++ concatMap funLocalsArgs ds

-- allowRenameLocals :: String -> [Decl] -> Maybe [Exported] -> [Loc Var]
allowRenameLocals modName decls topLevelToRename =
  let allVars     = concatMap funLocals decls
  in partition (allowToRenameVar modName topLevelToRename) allVars
 where
    -- A simple variable is allowed to be renamed if:
    -- * it's has no qualifier (TODO: handle it separately)
    -- * it's qualifier == module name and it's in the `topLevelToRename` list
    allowToRenameVar _  _ (Loc _ (Var _ NoQual)) = True
    allowToRenameVar m tltr (Loc loc (Var var qual))
      | sameQualMod m qual
      , var `elem` tltr
      = True
    allowToRenameVar _ _ _ = False

-- | Collect top level symbols allowed to rename
--
-- Note: forbiden to rename data constructors, classes and instances.
allowRenameTopLevel :: String -> [Decl] -> Maybe [Exported] -> [String]
allowRenameTopLevel modName decls exps =
  let topLevelFun  = concatMap toplevelFun decls
      topLevelCtrs = concatMap toplevelCtrs decls
  in case exps of
        Nothing -> []
        Just exps ->
          filter (not . isExported exps) $ topLevelFun ++ topLevelCtrs
               -- filter (not . isExportedFun exps) topLevelFun ++
               -- filter (not . isExportedCtr exps) topLevelCtrs
 where
  toplevelFun FunD { fun_name = name } = [lcelem name]
  toplevelFun _                        = []

  -- instances are not allowed
  toplevelCtrs DataD { dt_ctrs = cons } = concatMap cn_fields cons
  toplevelCtrs ClassD { cls_funs = ns } = ns
  toplevelCtrs _                        = []

  isExported [] _ = False
  isExported (ExportedFun var : es) name
    | var == Var name (PQual modName)
    = True
  isExported (ExportedTyCl { e_ctrs = cs, e_fields = fs } : es) name
    | Var name (PQual modName) `elem` fs || Var name (PQual modName) `elem` cs
    = True
  isExported (_ : es) n = isExported es n

--
-- Note: all instances are exported in any case, but they're not
--       in the export list that can be obtained from `RenamedSource`
--       [(LIE GhcRn, GHC.Avails)].
--
-- The list [Exported] must be extended earlier or the note just must be
-- taken into account.
--
-- TODO: * hiding
--       * orphan instances
collectExportedSym :: [(LIE GhcRn, GHC.Avails)] -> [Exported]
collectExportedSym = mapMaybe (handleExports . first unLoc)
 where
  iename   = nameToVar' . lieWrappedName
  iefields = nameToVar' . GHC.flSelector

  handleExports :: (IE GhcRn, GHC.Avails) -> Maybe Exported
  handleExports (IEVar _ name, _) = Just $ ExportedFun $ iename name
  handleExports (IEThingAbs _ name, _) =
    Just $ ExportedTyCl (iename name) [] []
  handleExports (IEThingAll _ name, as) =
    let (ctrs, fs) = handleAvails as
    in  Just $ ExportedTyCl (iename name) ctrs fs
  handleExports (IEThingWith _ name _ _ _, as) =
    let (ctrs, fs) = handleAvails as
    in  Just $ ExportedTyCl (iename name) ctrs fs
  handleExports _ = Nothing

  handleAvails []                 = ([], [])
  handleAvails (GHC.Avail _ : as) = handleAvails as
  handleAvails (GHC.AvailTC name ctrs fields : as) =
    let (cs, fs) = handleAvails as
        ctrs'    = nameToVar' <$> delete name ctrs
        fields'  = iefields <$> fields
    in  (ctrs' ++ cs, fields' ++ fs)

-- | Collect all top level declarations.
collectDecls :: HsModule GhcPs -> [Decl]
collectDecls = mapMaybe (handleDecl . unLoc) . hsmodDecls
 where
  handleDecl (ValD _ (FunBind _ name matches _ _)) =
    Just $ toFunDecl name matches
  handleDecl (TyClD _ DataDecl { tcdLName = name, tcdDataDefn = defn }) =
    Just $ toDataDecl name defn
  handleDecl (TyClD _ ClassDecl { tcdLName = L _ name, tcdSigs = sigs }) =
    Just $ toClassDecl name sigs
  handleDecl (InstD _ ClsInstD { cid_inst = cinst }) =
    Just $ toInstDecl cinst
  -- Maybe, support later
  -- handleDecl (SigD _ (TypeSig _ names _))  = (SigDecl . rdrnameToDef) <$> names
  -- handleDecl (TyClD _ SynDecl { tcdLName = name }) = Just $ TypeDecl $ rdrnameToDef name
  handleDecl _ = Nothing

  toInstDecl :: ClsInstDecl GhcPs -> Decl
  toInstDecl ClsInstDecl { cid_binds = binds } =
    InstanceD $ map (\(L _ (FunBind _ n m _ _)) -> toFunDecl n m) $ GHC.bagToList binds

  toClassDecl name ss = ClassD (rdrName2String name)
                               (concatMap collectNames ss)
   where
    collectNames (L _ (ClassOpSig _ _ ns _)) = rdrName2String . unLoc <$> ns
    collectNames _                           = []

  toDataDecl name HsDataDefn { dd_cons = cons } =
    DataD (rdrName2String $ unLoc name) $ map
      (\(L _ con) ->
        Con (rdrName2String $ unLoc $ con_name con) (fields $ con_args con)
      )
      cons
   where
    fields :: HsConDeclDetails GhcPs -> [String]
    fields (RecCon (L _ rc)) = concatMap
      ( map (rdrName2String . unLoc . rdrNameFieldOcc . unLoc)
      . cd_fld_names
      . unLoc
      )
      rc
    fields _ = []

  toFunDecl name mg =
    let
      name' = rdrName2String <$> toLoc name
      args =
        fmap (fmap rdrName2String . toLoc) $ unique $ concat $ collectArguments
          mg
      defs = collectInnerDefs mg
      vars = rdrnamesToVars $ collectInnerVars mg
    in
      FunD name' args defs vars

  collectArguments :: MatchGroup GhcPs (LHsExpr GhcPs) -> [[Located RdrName]]
  collectArguments = fmap (collectArguments' . unLoc) . unLoc . mg_alts

  collectArguments' :: Match GhcPs (LHsExpr GhcPs) -> [Located RdrName]
  collectArguments' (Match _ _ pat _) = collect arg pat
   where
    arg :: Pat GhcPs -> [Located RdrName]
    arg (VarPat _ name) = [name]
    arg _               = []

  collectInnerDefs :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Decl]
  collectInnerDefs = collect innerDefs
   where
    innerDefs :: HsBind GhcPs -> [Decl]
    innerDefs (FunBind _ name matches _ _) = [toFunDecl name matches]
    innerDefs _                            = []

  collectInnerVars :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Located RdrName]
  collectInnerVars = collect var
   where
    var :: HsExpr GhcPs -> [Located RdrName]
    var (HsVar _ var) = [var]
    var _             = []
