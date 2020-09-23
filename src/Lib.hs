{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveFunctor #-}

module Lib where
    
import Language.Haskell.GHC.ExactPrint as EP
import Language.Haskell.GHC.ExactPrint.Parsers as EP
import Language.Haskell.GHC.ExactPrint.Utils as EP

import Data.Generics as SYB

import GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified Bag as GHC
import qualified Name as GHC
import qualified Module as GHC

import Data.Maybe
import Data.List

import Control.Arrow ((&&&))

import GHC.Paths (libdir)

import DynFlags

import qualified Outputable as Out

import Debug.Trace

data Var = Var { varname :: String, varqual :: Maybe String }
  deriving Eq

instance Show Var where
  show (Var n Nothing) = show n
  show (Var n (Just q)) = show (q ++ "." ++ n)

data Def = Def { defname :: String }
  deriving Eq

instance Show Def where
   show = show . defname

data TopLevelDef = TLDef {
    tldefname :: Loc String,
    tldefargs :: [Loc String],
    tldefdefs :: [Loc Def],
    tldefvars :: [Loc Var]
  }

instance Show TopLevelDef where
  show (TLDef n a d v) = "Name: " ++ lcelem n ++
                         "\n-- Args: " ++ show (lcelem <$> a) ++
                         "\n-- Defs: " ++ show (lcelem <$> d) ++
                         "\n-- Vars: " ++ show (lcelem <$> v)

data Loc a = Loc { lcloc :: SrcSpan, lcelem :: a }
  deriving (Eq, Show, Functor)

type Changer = (Anns -> ParsedSource -> IO (Anns,ParsedSource))

putList :: Show a => [a] -> IO ()
putList = putStrLn . intercalate "\n" . map show


changeLetIn1 :: Changer
changeLetIn1 ans parsed = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
   replace :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
--   replace (GHC.HsLet _ (GHC.L lb localDecls) expr@(GHC.L _ _)) =
--     let (GHC.HsValBinds x (GHC.ValBinds xv bagDecls sigs)) = localDecls
--         bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
--     in (GHC.HsLet noExt (GHC.L lb (GHC.HsValBinds x (GHC.ValBinds xv bagDecls' sigs))) expr)
   replace (HsVar ext name) = HsVar ext ((\_ -> GHC.mkRdrUnqual (GHC.mkVarOcc "XXXX")) <$> name)
   replace x = x

parse = EP.parseModule

-- | Collect a list exported identifiers.
-- Result:
--  Nothing -> no export list => export all
--  Just [] -> export nothing
--  Just [..] -> export a list of names
collectExportedSym :: HsModule GhcPs -> Maybe [String]
collectExportedSym mod = catMaybes <$> (\a -> (handleExports . unLoc) <$> unLoc a) <$> hsmodExports mod
  where
    handleExports :: IE GhcPs -> Maybe String
    handleExports (IEVar _ name) = Just $ rdrName2String $ lieWrappedName name
    handleExports _ = Nothing

generateObfuscatedNames = generateObfuscatedNames' 1

generateObfuscatedNames'  :: Int -> [String] -> [String]
generateObfuscatedNames' n =
    map (\(n, name) -> gen n name) .
    zip [n..]
  where
    gen n _ = take n ['a','a'..]

unique :: Ord a => [a] -> [a]
unique = fmap head . group . sort

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------

namesToVars :: [Located Name] -> [Loc Var]
namesToVars = map nameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

nameToVar :: Located Name -> Loc Var
nameToVar name = let (nm, qual) = destructName $ unLoc name
                     symb = Var { varqual = qual, varname = nm }
                 in Loc (getLoc name) symb

destructName :: Name -> (String, Maybe String)
destructName name =
  let qual = (GHC.moduleNameString . GHC.moduleName) <$> GHC.nameModule_maybe (unLoc name)
      nm = GHC.occNameString $ GHC.nameOccName $ unLoc name
  in (nm, qual)

rdrnamesToVars :: [Located RdrName] -> [Loc Var]
rdrnamesToVars = map rdrnameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

rdrnameToVar :: Located RdrName -> Loc Var
rdrnameToVar name =
  let (nm, qual) = f $ unLoc name
      symb = Var { varqual = qual, varname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Qual mn occnm) = (GHC.occNameString occnm, Just $ GHC.moduleNameString mn)
    f (GHC.Orig m occnm) = (GHC.occNameString occnm, Just $ GHC.moduleNameString $ GHC.moduleName $ m)
    f (GHC.Exact nm) = destructName nm
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, Nothing)


rdrnameToDef :: Located RdrName -> Loc Def
rdrnameToDef name = 
  let (nm, _) = f $ unLoc name
      symb = Def { defname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, Nothing)
    f _ = error "Impossible: qualified name in binding"

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------

collectLocatedRenamedNames :: HsGroup GhcRn -> [Located Name]
collectLocatedRenamedNames = SYB.everything (++) ([] `SYB.mkQ` t1)
  where
    t1 :: HsExpr GhcRn -> [Located Name]
    t1 (HsVar ext var) = [var]
    t1 x = []

collectVariables :: HsModule GhcPs -> [Located RdrName]
collectVariables = SYB.everything (++) ([] `SYB.mkQ` t1)
  where
    t1 :: HsExpr GhcPs -> [Located RdrName]
    t1 (HsVar ext var) = [var]
    t1 _ = []

collectFunBindNames :: HsModule GhcPs -> [Located RdrName]
collectFunBindNames = SYB.everything (++) ([] `SYB.mkQ` t1)
  where
    t1 :: HsBind GhcPs -> [Located RdrName]
    t1 (FunBind _ name _ _ _) = [name]
    t1 _ = []

collectPatNames :: HsModule GhcPs -> [Located RdrName]
collectPatNames = SYB.everything (++) ([] `SYB.mkQ` t1)
  where
    t1 :: Pat GhcPs -> [Located RdrName]
    -- t1 (FunBind _ name _ _ _) = [name]
    t1 (VarPat _ name) = [name]
    t1 _ = []

-- collectSymbols :: HsModule GhcPs -> [(Def, [Var])]
collectAllSymbols mod =
  let varsNames = collectVariables mod
      vars = rdrnamesToVars varsNames
      defsNames = collectFunBindNames mod
      patNames = collectPatNames mod
      defs = rdrnameToDef <$> (defsNames <> patNames)
  in (vars, defs)

collect f = SYB.everything (++) ([] `SYB.mkQ` f)

collectBut f p = collectBut' (f &&& p)

collectBut' f = SYB.everythingBut (++) (([], False) `SYB.mkQ` f)

collectTopLevelBindings :: HsModule GhcPs -> [TopLevelDef]
collectTopLevelBindings =
  fmap toTopLevelDef . collectBut' f
  where
    toTopLevelDef (name, mg) =
      let
        name' = rdrName2String <$> toLoc name
        args = (fmap rdrName2String . toLoc) <$> (unique $ concat $ collectArguments mg)
        defs = fmap rdrnameToDef $ collectInnerDefs mg
        vars = rdrnamesToVars $ collectInnerVars mg
      in TLDef name' args defs vars

    f :: HsBind GhcPs -> ([(Located RdrName, MatchGroup GhcPs (LHsExpr GhcPs))], Bool)
    f (FunBind _ name matches _ _) = ([(name, matches)], True)
    f _ = ([], False)

toLoc a = Loc (getLoc a) (unLoc a)

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

collectTest' :: HsModule GhcPs -> [Located RdrName]
collectTest' = SYB.everythingWithContext [] (++) (mkQ (\s -> ([], s)) f)
  where
    f :: HsBind GhcPs -> [Located RdrName] -> ([Located RdrName], [Located RdrName])
    f (FunBind _ name _ _ _) ctx = ([name], name : ctx)
    f _ ctx = ([], ctx)

collectTest :: HsModule GhcPs -> [(Located RdrName, [Located RdrName], [Located RdrName])]
collectTest = collect bind
  where
    bind :: HsBind GhcPs -> [(Located RdrName, [Located RdrName], [Located RdrName])]
    bind (FunBind _ name matches _ _) =
      let args = collect arg matches
          vars = collect var matches
      in [(name, args, vars)]
    bind _ = []

    stopBind = undefined

    arg :: Pat GhcPs -> [Located RdrName]
    arg (VarPat _ name) = [name]
    arg _ = []

    var :: HsExpr GhcPs -> [Located RdrName]
    var (HsVar ext var) = [var]
    var _ = []


getModuleName :: HsModule GhcPs -> Maybe String
getModuleName = fmap (moduleNameString . unLoc) . hsmodName

example path mod = defaultErrorHandler defaultFatalMessager defaultFlushOut $
  do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget path Nothing
      setTargets [target]
      load LoadAllTargets
      modSum <- getModSummary $ mkModuleName mod
      p <- GHC.parseModule modSum
      t <- typecheckModule p
      -- let (env, _) = tm_internals_ t
      -- return env
      return $ tm_renamed_source t

-- Get imported symbols
-- fmap (fmap (fmap rdrName2String)) $ fmap (fmap greRdrNames) $ occEnvElts $ tcg_rdr_env env

{-
testM = do
  let path = "resources/t1.hs"
  Just rsrc <- example path "T1"
  Right (ans, src) <- EP.parseModule path
  let (group, _, _, _) = rsrc
  --let rns = namesToVars $ collectLocatedRenamedNames group
  --let pns = rdrnameToVar $ collectVariables $ unLoc src
  --let fbns = collectFunBindNames $ unLoc src
  --let pns = fmap rdrnameToDef fbns
  --let frns = (\nm -> fmap rdrnameToDef $ collectFunRhsNames nm $ unLoc src) <$> fbns
  --putStrLn "Definitions"
  --putStrLn $ intercalate "\n" $ show <$> pns
  --putStrLn "============"
  --putStrLn "FunRhs"
  --putStrLn $ intercalate "\n" $ show <$> frns
  --putStrLn "============"
  --putStrLn $ intercalate "\n" $ show <$> rns

  let rns = namesToVars $ collectLocatedRenamedNames group
  let rns' = filter (isNothing . varqual . lcelem) rns
  let names = generateObfuscatedNames $ fmap (varname . lcelem) rns'
  putStrLn $ intercalate "\n" $ show <$> rns'
  putStrLn $ show names
  let renamings = zip rns' names
  (ans, src) <- changeVar ans src renamings
  (ans, src) <- changeBind ans src renamings
  (ans, src) <- changeMatch ans src renamings
  putStrLn $ exactPrint src ans
-}

change changer ans parsed = return (ans,SYB.everywhere (SYB.mkT changer) parsed)

-- change1 :: Changer
changeVar renamings = change replaceVar
  where
    replaceVar :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
    replaceVar (HsVar ext name)
      | Just newName <- lookupRenaming renamings name
      = HsVar ext (newRdrName newName <$> name)
    replaceVar x = x

changeBind renamings = change replace
  where
   replace :: GHC.HsBind GhcPs -> GHC.HsBind GhcPs
   replace (FunBind a name b c d)
     | Just newName <- lookupRenaming renamings name
     = FunBind a (newRdrName newName <$> name) b c d
   replace x = x

changeMatch renamings = change replace
  where
   replace (FunRhs name a b)
     | Just newName <- lookupRenaming renamings name
     = FunRhs (newRdrName newName <$> name) a b
   replace x = x

changeFunction renamings ans src = do
   (ans, src) <- changeBind renamings ans src
   changeMatch renamings ans src

changePat renamings = change replace
  where
    replace :: GHC.Pat GhcPs -> GHC.Pat GhcPs
    replace (VarPat a name)
      | Just newName <- lookupRenaming renamings name
      = VarPat a (newRdrName newName <$> name)
    replace x = x

lookupRenaming [] _ = Nothing
lookupRenaming ((oldname, newname):rn) name
  | oldname == (GHC.occNameString $ GHC.rdrNameOcc $ unLoc name)
  = Just newname
  | otherwise
  = lookupRenaming rn name

newRdrName name (GHC.Unqual _) = GHC.Unqual (GHC.mkVarOcc name)
newRdrName name (GHC.Qual mod occ) = GHC.Qual mod (GHC.mkVarOcc name)
newRdrName name (GHC.Orig mod occ) = GHC.Orig mod (GHC.mkVarOcc name)
newRdrName _ _ = error "newRdrName: Exact ctr. How is it used?"

filterVarsWithSpan span = filter ((flip elem) span . lcloc)

test path = do
  Right (ans, src) <- EP.parseModule path
  let mod = unLoc src
  let modName = fromMaybe "Main" $ getModuleName mod
  let exported = collectExportedSym mod

  Just rsrc <- example path modName
  let (group, _, _, _) = rsrc
  let p1 = isNothing . varqual . lcelem
  let p2 = (== Just modName) . varqual . lcelem

  let renamedVars = namesToVars $ collectLocatedRenamedNames group
  let varSpans = fmap lcloc $ filter (\v -> p2 v || p1 v) renamedVars

  let (vars, defs) = collectAllSymbols mod
  let localVars = filterVarsWithSpan varSpans vars

  let varNames = fmap (varname . lcelem) localVars
  let defNames = fmap (defname . lcelem) defs
  let names = unique $ varNames <> defNames
  let renaming = zip names $ generateObfuscatedNames names

--  putStrLn "\nVars\n"
--  putList localVars
--  putStrLn "\nDefs\n"
--  putList defs

  (ans, src) <- changeVar renaming ans src
  (ans, src) <- changeBind renaming ans src
  (ans, src) <- changeMatch renaming ans src
  (ans, src) <- changePat renaming ans src

  putStrLn $ exactPrint src ans

test1 = test "resources/t1.hs"
test2 = test "resources/t2.hs"
test3 = test "resources/t3.hs"


testC path = do
  Right (ans, src) <- EP.parseModule path
  let mod = unLoc src
  let modName = fromMaybe "Main" $ getModuleName mod
  Just rsrc <- example path modName
  let (group, _, _, _) = rsrc
  let renamedVars = namesToVars $ collectLocatedRenamedNames group
  let exported = collectExportedSym mod

  let a = collectTopLevelBindings $ unLoc src
  let tldefs = addQualifications renamedVars <$> a

  let locals = concatMap allLocalNames tldefs
  let notExportedDefs = ((lcelem . tldefname) <$> tldefs) \\ (fromMaybe [] exported)
  let names = unique $ locals <> notExportedDefs
  let renamings = zip names $ generateObfuscatedNames names

  putList tldefs
  putStrLn $ show $ renamings

  (ans, src) <- rename modName tldefs renamings (fromMaybe [] exported) ans src
  putStrLn "========"
  putStrLn $ exactPrint src ans

rename modname tldefs renamings exported ans mod = do
  (ans, mod) <- change changerBind ans mod
  (ans, mod) <- change changerMatch ans mod
  (ans, mod) <- change changerPat ans mod
  (ans, mod) <- change changerVar ans mod
  return (ans, mod)
  where
   changerBind :: GHC.HsBind GhcPs -> GHC.HsBind GhcPs
   changerBind (FunBind a name b c d)
     | Just newName <- getNewNameDef name
     = FunBind a (newRdrName newName <$> name) b c d
   changerBind  x = x

   changerMatch :: HsMatchContext RdrName -> HsMatchContext RdrName
   changerMatch (FunRhs name a b)
     | Just newName <- getNewNameDef name
     = FunRhs (newRdrName newName <$> name) a b
   changerMatch x = x

   changerPat :: GHC.Pat GhcPs -> GHC.Pat GhcPs
   changerPat (VarPat a name)
     | Just newName <- lookupRenaming renamings name
     = VarPat a (newRdrName newName <$> name)
   changerPat x = x

   changerVar :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
   changerVar (HsVar ext name)
     | Just newName <- lookupRenaming renamings name
     = HsVar ext (newRdrName newName <$> name)
   changerVar x = x

   getNewNameDef name
     | loc <- getLoc name
     , name' <- rdrName2String $ unLoc name
     , Just _ <- lookupDef tldefs $ Loc loc name'
     , not $ name' `elem` exported
     = lookup name' renamings
     | loc <- getLoc name
     , name' <- rdrName2String $ unLoc name
     , Nothing <- lookupDef tldefs $ Loc loc name'
     = lookup name' renamings
     | otherwise = Nothing


lookupDef = lookupGen (\def name -> name == tldefname def) id

allLocalNames (TLDef _ args defs vars) =
   (lcelem <$> args)
   <> ((defname . lcelem) <$> defs)
   <> ((varname . lcelem) <$> (filter (isLocal . lcelem) vars))

isLocal (Var _ Nothing) = True
isLocal _ = False

isTotallyLocal modName (Var _ Nothing) = True
isTotallyLocal modName (Var _ (Just m)) = modName == m

addQualifications :: [Loc Var] -> TopLevelDef -> TopLevelDef
addQualifications vars (TLDef n a d vs) = TLDef n a d (update vars <$> vs)
  where
    update vars v@(Loc loc (Var name _))
      | Just q <- lookupVarByName vars v
      = Loc loc (Var name q)
      | otherwise
      = v

lookupVarByName [] _ = Nothing
lookupVarByName (Loc l (Var n q):vs) v@(Loc loc (Var name _))
  | l == loc, n == name = Just q
  | otherwise = lookupVarByName vs v

lookupGen _ _ [] _ = Nothing
lookupGen f p (x:xs) y
  | f x y = Just (p x)
  | otherwise = lookupGen f p xs y

instance Show (GenLocated SrcSpan RdrName) where
  show (L s r) = rdrName2String r
