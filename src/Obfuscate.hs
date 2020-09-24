{-# LANGUAGE TypeFamilies, FlexibleInstances, DeriveFunctor #-}

module Lib where
    
import Language.Haskell.GHC.ExactPrint as EP
import Language.Haskell.GHC.ExactPrint.Parsers as EP
import Language.Haskell.GHC.ExactPrint.Utils as EP

import qualified GHC.SourceGen as SG


import Data.Generics as SYB

import GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified Bag as GHC
import qualified Name as GHC
import qualified Module as GHC

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.String

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

toLoc a = Loc (getLoc a) (unLoc a)

data ObfContext = OC {
      ocModName :: String
    , ocTopLevelDefs :: [Loc String]
    , ocArgs :: [Loc String]
    , ocInnerDefs :: [Loc Def]
    , ocVars :: [Loc Var]
    , ocRenamings :: [(String, String)]
    , ocExported :: [String]
  }

putList :: Show a => [a] -> IO ()
putList = putStrLn . intercalate "\n" . map show

unique :: Ord a => [a] -> [a]
unique = fmap head . group . sort


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
    map (\(n, name) -> 'a' : gen n name) .
    zip [n..]
  where
    -- gen n _ = concatMap show $ take n [n..]
    gen n _ = take n ['a', 'a' ..]

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

collect f = SYB.everything (++) ([] `SYB.mkQ` f)

collectBut f p = collectBut' (f &&& p)

collectBut' f = SYB.everythingBut (++) (([], False) `SYB.mkQ` f)

collectLocatedRenamedNames :: HsGroup GhcRn -> [Located Name]
collectLocatedRenamedNames = collect t1
  where
    t1 :: HsExpr GhcRn -> [Located Name]
    t1 (HsVar ext var) = [var]
    t1 x = []

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

getModuleName :: HsModule GhcPs -> Maybe String
getModuleName = fmap (moduleNameString . unLoc) . hsmodName

change changer ans parsed = return (ans,SYB.everywhere (SYB.mkT changer) parsed)
changeBut stopper changer ans parsed = return (ans,SYB.everywhereBut (SYB.mkQ False stopper) (SYB.mkT changer) parsed)

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

lookupVar :: [Loc Var] -> Loc String -> Maybe (Loc Var)
lookupVar = lookupGen (\var name -> (lcloc name == lcloc var) &&
                                    ((varname $ lcelem var) == lcelem name))id
lookupDef = lookupGen (\def name -> name == tldefname def) id

allLocalSymbols args defs vars =
   (lcelem <$> args)
   <> ((defname . lcelem) <$> defs)
   <> ((varname . lcelem) <$> (filter (isLocal . lcelem) vars))

isLocal (Var _ Nothing) = True
isLocal _ = False

addQualifications :: [Loc Var] -> TopLevelDef -> TopLevelDef
addQualifications vars (TLDef n a d vs) = TLDef n a d (update vars <$> vs)
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

lookupGen _ _ [] _ = Nothing
lookupGen f p (x:xs) y
  | f x y = Just (p x)
  | otherwise = lookupGen f p xs y

-- For debug pursposes
instance Show (GenLocated SrcSpan RdrName) where
  show (L s r) = rdrName2String r

rename octx ans mod = do
  (ans, mod) <- change changerBind ans mod
  (ans, mod) <- change changerMatch ans mod
  (ans, mod) <- change changerPat ans mod
  (ans, mod) <- change changerVar ans mod
  return (ans, mod)
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
     | Just newName <- getRenaming octx name
     = VarPat a (newRdrName newName <$> name)
     -- = VarPat a (noLoc $ newRdrName newName $ unLoc name)
   changerPat x = x

   changerVar :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
   changerVar (HsVar ext name)
     | Just newName <- getNewNameVar octx name
     = HsVar ext (newRdrName newName <$> name)
     -- = HsVar ext (noLoc $ newRdrName newName $ unLoc name)
   changerVar x = x

   getRenaming octx = lookupRenaming (ocRenamings octx)

   getNewNameVar octx name
     | Loc loc (Var name' qual') <- rdrnameToVar name
     , Just (Loc _ (Var _ qual)) <- lookupVar (ocVars octx) (Loc loc name')
     , (fromMaybe "" qual == (ocModName octx) && not (name' `elem` (ocExported octx)) || isNothing qual)
     , Just newName <- getRenaming octx name
     = Just newName
     | otherwise = Nothing

   getNewNameDef octx name
     | loc    <- getLoc name
     , name'  <- rdrName2String $ unLoc name
     , Just _ <- find (== Loc loc name') (ocTopLevelDefs octx)
     , not $ name' `elem` (ocExported octx)
     = lookup name' (ocRenamings octx)
     | loc     <- getLoc name
     , name'   <- rdrName2String $ unLoc name
     , Nothing <- find (== Loc loc name') (ocTopLevelDefs octx)
     = lookup name' (ocRenamings octx)
     | otherwise = Nothing

-- transform
transformOpToApp :: HsExpr GhcPs -> HsExpr GhcPs
transformOpToApp (OpApp _ left op right) = HsApp noExt (noLoc $ HsPar noExt $ noLoc (HsApp noExt op left)) right
transformOpToApp x = x

createDecl :: String -> String -> HsDecl GhcPs
createDecl name call = SG.funBind (fromString name) $ SG.match [] $ SG.var $ fromString call

addDecl :: HsDecl GhcPs -> HsModule GhcPs  -> HsModule GhcPs
addDecl d (HsModule n e i ds x y) = (HsModule n e i (ds ++ [noLoc d]) x y)

renameImportedSymbols :: ObfContext -> Anns -> Located (HsModule GhcPs) -> IO (Anns, Located (HsModule GhcPs))
renameImportedSymbols octx ans src = do
    (ans, src) <- change changerImported ans src
    let decls = uncurry f <$> importedRenamings
    let src' = foldl (\s decl -> addDecl decl <$> s) src decls
    return (ans, src')
  where
    importedSymbols = filter ((\q -> isJust q && fromJust q /= ocModName octx) . varqual . lcelem) $ ocVars octx
    importedRenamings = zip importedSymbols $
                            generateObfuscatedNames' (length $ ocRenamings octx) $ fmap (varname . lcelem) importedSymbols

    f (Loc _ var) newName = createDecl newName $ varname var

    changerImported :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
    changerImported (HsVar ext name)
      | Just newName <- getNewName name
      = HsVar ext (newRdrName newName <$> name)
      -- = HsVar ext (noLoc $ newRdrName newName $ unLoc name)
    changerImported x = x

    getNewName name = lookupIR (rdrnameToVar name) importedRenamings

    lookupIR _ [] = Nothing
    lookupIR n@(Loc loc var) (((Loc loc' var'), newName):irs)
      | loc == loc'
      , varname var == varname var'
      = Just newName
      | otherwise = lookupIR n irs

getRenamedSource path mod = defaultErrorHandler defaultFatalMessager defaultFlushOut $
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
      return $ (tm_renamed_source t, dflags)

obfuscate path = do
  Right (ans, src) <- EP.parseModule path
  let mod = unLoc src
  let modName = fromMaybe "Main" $ getModuleName mod
  (Just rsrc, dflags) <- getRenamedSource path modName
  let (group, _, _, _) = rsrc
  let renamedVars = namesToVars $ collectLocatedRenamedNames group
  let exported = fromMaybe [] $ collectExportedSym mod

  let a = collectTopLevelBindings $ unLoc src
  let tldefs = addQualifications renamedVars <$> a
  let d@(topLevelDefs, args, innerDefs, vars) = foldl' (\(ds, as, vs, is) (TLDef d a v i) -> (d : ds, a ++ as , v ++ vs, i ++ is)) ([], [], [], []) tldefs

  let locals = allLocalSymbols args innerDefs vars
  let notExportedDefs = (lcelem <$> topLevelDefs) \\ exported
  let names = unique $ locals <> notExportedDefs
  let renamings = zip names $ generateObfuscatedNames names

  let octx = OC modName topLevelDefs args innerDefs vars renamings exported

--  putList tldefs
--  putStrLn $ show $ renamings

  (ans, src) <- rename octx ans src
  (ans, src) <- change transformOpToApp ans src
  (ans, src) <- renameImportedSymbols octx ans src
  -- putList $ Map.toList ans
  --let code = Out.showSDocUnsafe $ Out.ppr src
  let code = Out.showSDoc dflags $ Out.ppr src
  -- let code = Out.showSDocOneLine dflags $ Out.ppr src
  --putStrLn "========"
  -- let code = exactPrint src ans
  putStrLn code
  -- writeFile (path ++ ".obf") code
