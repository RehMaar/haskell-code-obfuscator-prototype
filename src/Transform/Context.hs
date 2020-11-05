{-# LANGUAGE TypeFamilies #-}
module Transform.Context
  -- (TransformContext(..), initTransformContext)
  where
    
import Language.Haskell.GHC.ExactPrint.Utils as EP

import GHC
import qualified Avail as GHC
import qualified FieldLabel as GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified Bag as GHC
import qualified Name as GHC
import qualified Module as GHC
import qualified FastString as GHC
import qualified BasicTypes as GHC

import Data.Maybe
import Data.List
import Control.Arrow (first)

import Transform.Types
import Transform.Query
import Source
import Utils

-- TODO: rename (maybe SourceContext)
data TransformContext = TC {
      tcModName :: String
    , tcTopLevelDefs :: [Loc String]
    , tcArgs :: [Loc String]
    , tcInnerDefs :: [Loc Def]
    , tcVars :: [Loc Var]
    -- , tcRenamings :: [(String, String)]
    , tcNames :: [String]
    , tcExported :: [String]
    , tcInternal_ :: [TopLevelDef]
  }
  -- deriving Show

instance Show TransformContext where
  show tc = ">> ModName:\n"  ++ show (tcModName tc)      ++ "\n" ++
            ">> Defs:\n"     ++ showL (tcTopLevelDefs tc) ++ "\n" ++
            ">> Args:\n"     ++ showL (tcArgs tc)         ++ "\n" ++
            ">> Inners:\n"   ++ showL (tcInnerDefs tc)    ++ "\n" ++
            ">> Vars:\n"     ++ showL (tcVars tc)         ++ "\n" ++
            ">> Names:\n"    ++ showL (tcNames tc)        ++ "\n" ++
            ">> Exported:\n" ++ showL (tcExported tc)     ++ "\n" ++
            show (tcInternal_ tc)

showL xs = intercalate "\n" (map show xs)



initTransformContext :: [Located Name] -> HsModule GhcPs -> TransformContext
initTransformContext rvs mod =
  let tldefs = addQualInTld (namesToVars rvs) <$> collectTopLevelBindings mod
      (topLevelDefs, args, innerDefs, vars) = foldl' collectTldInfo ([], [], [], []) tldefs

      exported = "main" : fromMaybe [] (collectExportedSym mod)
      symbols = allSymbols args innerDefs vars
      notExportedDefs = (lcelem <$> topLevelDefs) \\ exported
      names = unique $ symbols <> notExportedDefs

      modName   = fromMaybe "Main" $ getModuleName mod
      -- renamings = zip names $ generateObfuscatedNames names
  in TC {
      tcModName = modName,
      tcTopLevelDefs = topLevelDefs,
      tcArgs = args,
      tcInnerDefs = innerDefs,
      tcVars = vars,
      -- tcRenamings = renamings,
      tcNames = names,
      tcExported = exported,
      tcInternal_ = tldefs }
  where
    addQualInTld rvs t@TLDef{ tldefvars = vs } = t { tldefvars = addQualifications rvs vs }
    collectTldInfo (ds, as, vs, is) (TLDef d a v i) = (d : ds, a ++ as , v ++ vs, i ++ is)


-- | Collect a list exported identifiers.
-- Result:
--  Nothing -> no export list => export all
--  Just [] -> export nothing
--  Just [..] -> export a list of names
collectExportedSym :: HsModule GhcPs -> Maybe [String]
collectExportedSym mod = mapMaybe (handleExports . unLoc) . unLoc <$> hsmodExports mod
  where
    handleExports :: IE GhcPs -> Maybe String
    handleExports (IEVar _ name) = Just $ rdrName2String $ lieWrappedName name
    handleExports _ = Nothing

collectTopLevelBindings :: HsModule GhcPs -> [TopLevelDef]
collectTopLevelBindings =
  fmap toTopLevelDef . collectBut' funB
  where
    toTopLevelDef (name, mg) =
      let
        name' = rdrName2String <$> toLoc name
        args = fmap (fmap rdrName2String . toLoc) $ unique $ concat $ collectArguments mg
        defs = rdrnameToDef <$> collectInnerDefs mg
        vars = rdrnamesToVars $ collectInnerVars mg
      in TLDef name' args defs vars

    funB :: HsBind GhcPs -> ([(Located RdrName, MatchGroup GhcPs (LHsExpr GhcPs))], Bool)
    funB (FunBind _ name matches _ _) = ([(name, matches)], True)
    funB _ = ([], False)

    collectArguments' :: Match GhcPs (LHsExpr GhcPs) -> [Located RdrName]
    collectArguments' (Match _ _ pat _) = collect arg pat
      where
        arg :: Pat GhcPs -> [Located RdrName]
        arg (VarPat _ name) = [name]
        arg _ = []

    collectArguments :: MatchGroup GhcPs (LHsExpr GhcPs) -> [[Located RdrName]]
    collectArguments =  fmap (collectArguments' . unLoc) . unLoc . mg_alts

    collectInnerDefs :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Located RdrName]
    collectInnerDefs = collect innerDefs
      where
        innerDefs :: HsBind GhcPs -> [Located RdrName]
        innerDefs (FunBind _ name _ _ _) = [name]
        innerDefs _ = []

    collectInnerVars :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Located RdrName]
    collectInnerVars = collect var
      where
        var :: HsExpr GhcPs -> [Located RdrName]
        var (HsVar _ var) = [var]
        var _ = []

addQualifications :: [Loc Var] -> [Loc Var] -> [Loc Var]
addQualifications vars = fmap (update vars)
  where
    update vars v@(Loc loc (Var name _))
      | Just q <- findQual vars v
      = Loc loc (Var name q)
      | otherwise
      = v

    findQual [] _ = Nothing
    findQual (Loc l (Var n q):vs) v@(Loc loc (Var name _))
      | l == loc, n == name = Just q
      | otherwise = findQual vs v

allSymbols args defs vars =
   (lcelem <$> args)
   <> (defname . lcelem <$> defs)
   <> (varname . lcelem <$> vars)

-- WIP
{-
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
  , sc_allow_rename :: [Loc Var]
  }
  deriving Show

data Exported
  = ExportedTyCl { e_typeName :: Var, e_ctrs :: [Var], e_fields :: [Var] }
  | ExportedFun  { e_funName :: Var }
  deriving Show

data Con = Con { cn_name :: String, cn_fields :: [String] }
  deriving Show

data Decl
 = DataD  { dt_name :: String, dt_ctrs :: [Con] }
 | ClassD { cls_name :: String, cls_funs :: [String] }
 | FunD   { fun_name :: Loc String
             , fun_args :: [Loc String]
             , fun_innerDecls :: [Decl]
             , fun_vars :: [Loc Var]}
 -- Probably, need it later
 -- | TypeDecl  { tp_name :: Loc Def  }
 -- | InstDecl  { inst_className :: String, inst_typeName :: String, inst_funs :: [String] }
 -- | SigDecl   { decl_name :: Loc Def}
  deriving Show

initSC :: SourceInfo -> SourceContext
initSC (SourceInfo _ src rvs exports _ ) = let
    modName   = fromMaybe "Main" $ getModuleName $ unLoc src
    decls = collectDecls $ unLoc src
    exported = (collectExportedSym' <$> exports)
    decls' = addQualifications' (namesToVars rvs) decls
    allow_rename = collectAllowRename modName decls exported
  in SC { sc_module_name = modName
        , sc_decls = decls
        , sc_exported = exported
        , sc_allow_rename = undefined
        }

addQualifications' :: [Loc Var] -> [Decl] -> [Decl]
addQualifications' = undefined

collectAllowRename :: String -> [Decl] -> Maybe [Exported] -> [Loc Var]
collectAllowRename modName decls exported = let
    topLevelToRename = fromMaybe [] (allowRenameTopLevel modName decls <$> exported)
    localsToRename   = allowRenameLocals modName exported decls
  in undefined

allowRenameLocals modName exps decls = undefined
  where
    funLocals FunD { fun_args = args, fun_innerDecls = idecls, fun_vars = vars } = undefined


-- | Collect top level symbols allowed to rename
--
-- TODO: does not include data constructors
allowRenameTopLevel :: String -> [Decl] -> [Exported] -> [String]
allowRenameTopLevel modName decls exps = let
    topLevelFun  = filter (not . isExportedFun exps) $ concatMap toplevelFun decls
    topLevelCtrs = filter (not . isExportedCtr exps) $ concatMap toplevelCtrs decls
    notExportedDefs = undefined
    notExportedVars = undefined
  in topLevelCtrs ++ topLevelFun
  where
    toplevelFun FunD { fun_name = name } = [lcelem name]
    toplevelFun _ = []

    toplevelCtrs DataD { dt_ctrs = cons } = concatMap cn_fields cons
    toplevelCtrs ClassD { cls_funs = ns } = ns
    toplevelCtrs _ = []

    isExportedFun [] _ = False
    isExportedFun (ExportedFun var : es) name
      = undefined
      -- | var == Var name (Just modName)
      -- = True
    isExportedFun (_:es) name = isExportedFun es name

    isExportedCtr [] _ = False
    isExportedCtr (ExportedTyCl { e_ctrs = cs, e_fields = fs } : es) name
      = undefined
      -- | Var name (Just modName) `elem` fs || Var name (Just modName) `elem` cs
      -- = True
    isExportedCtr (_:es) n = isExportedCtr es n

allowRenameVars = undefined

collectExportedSym' :: [(LIE GhcRn, GHC.Avails)] -> [Exported]
collectExportedSym' = mapMaybe (handleExports . first unLoc)
  where
    iename = nameToVar' . lieWrappedName
    iefields = nameToVar' . GHC.flSelector

    handleExports :: (IE GhcRn, GHC.Avails) -> Maybe Exported
    handleExports (IEVar _ name, _) = Just $ ExportedFun $ iename name
    handleExports (IEThingAbs _ name, _) = Just $ ExportedTyCl (iename name) [] []
    handleExports (IEThingAll _ name, as) =
      let (ctrs, fs) = handleAvails as
      in Just $ ExportedTyCl (iename name) ctrs fs
    handleExports (IEThingWith _ name _ _ _, as) =
      let (ctrs, fs) = handleAvails as
      in Just $ ExportedTyCl (iename name) ctrs fs
    handleExports _ = Nothing

    handleAvails [] = ([], [])
    handleAvails (GHC.Avail _ :as) = handleAvails as
    handleAvails (GHC.AvailTC name ctrs fields:as) =
      let (cs, fs) = handleAvails as
          ctrs' = nameToVar' <$> delete name ctrs
          fields' = iefields <$> fields
      in  (ctrs' ++ cs, fields' ++ fs)

collectDecls = mapMaybe (handleDecl . unLoc) . hsmodDecls
  where
    handleDecl (ValD _ (FunBind _ name matches _ _)) = Just $ toFunDecl name matches
    handleDecl (TyClD _ DataDecl { tcdLName = name, tcdDataDefn = defn }) = Just $ toDataDecl name defn
    handleDecl (TyClD _ ClassDecl { tcdLName = L _ name, tcdSigs = sigs }) = Just $ toClassDecl name sigs
    -- Maybe, support later
    -- handleDecl (SigD _ (TypeSig _ names _))  = (SigDecl . rdrnameToDef) <$> names
    -- handleDecl (TyClD _ SynDecl { tcdLName = name }) = Just $ TypeDecl $ rdrnameToDef name
    handleDecl _ = Nothing

    toClassDecl name ss = ClassD (rdrName2String name) (concatMap collectNames ss)
      where
        collectNames (L _ (ClassOpSig _ _ ns _)) = (rdrName2String . unLoc) <$> ns
        collectNames _  = []

    toDataDecl name HsDataDefn { dd_cons = cons } =
      DataD (rdrName2String $ unLoc name) $
        map
        (\(L _ con) -> Con (rdrName2String $ unLoc $ con_name con) (fields $ con_args con))
        cons
       where
         fields :: HsConDeclDetails GhcPs -> [String]
         fields (RecCon (L _ rc)) = concatMap (map (rdrName2String . unLoc . rdrNameFieldOcc . unLoc) . cd_fld_names . unLoc) rc
         fields _ = []

    toFunDecl name mg =
      let
        name' = rdrName2String <$> toLoc name
        args = fmap (fmap rdrName2String . toLoc) $ unique $ concat $ collectArguments mg
        defs = collectInnerDefs mg
        vars = rdrnamesToVars $ collectInnerVars mg
      in FunD name' args defs vars

    collectArguments :: MatchGroup GhcPs (LHsExpr GhcPs) -> [[Located RdrName]]
    collectArguments =  fmap (collectArguments' . unLoc) . unLoc . mg_alts

    collectArguments' :: Match GhcPs (LHsExpr GhcPs) -> [Located RdrName]
    collectArguments' (Match _ _ pat _) = collect arg pat
      where
        arg :: Pat GhcPs -> [Located RdrName]
        arg (VarPat _ name) = [name]
        arg _ = []

    collectInnerDefs :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Decl]
    collectInnerDefs = collect innerDefs
      where
        innerDefs :: HsBind GhcPs -> [Decl]
        innerDefs (FunBind _ name matches _ _) = [toFunDecl name matches]
        innerDefs _ = []

    collectInnerVars :: MatchGroup GhcPs (LHsExpr GhcPs) -> [Located RdrName]
    collectInnerVars = collect var
      where
        var :: HsExpr GhcPs -> [Located RdrName]
        var (HsVar _ var) = [var]
        var _ = []
-}
