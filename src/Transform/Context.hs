{-# LANGUAGE TypeFamilies #-}
module Transform.Context (TransformContext(..), initTransformContext) where
    
import Language.Haskell.GHC.ExactPrint.Utils as EP

import GHC
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

import Transform.Types
import Transform.Query
import Utils

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
  deriving Show

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
