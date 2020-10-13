{-# LANGUAGE DeriveFunctor #-}
module Transform.Types where

import GHC
import SrcLoc as GHC
import qualified OccName as GHC
import qualified RdrName as GHC

import Utils

data Var = Var { varname :: String, varqual :: Maybe String }
  deriving Eq

qulifiedVar (Var name Nothing) = name
qulifiedVar (Var name (Just qual)) = qual <> "." <> name

instance Show Var where
  show (Var n Nothing) = show n
  show (Var n (Just q)) = show (q ++ "." ++ n)


newtype Def = Def { defname :: String }
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

namesToVars :: [Located Name] -> [Loc Var]
namesToVars = map nameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

nameToVar :: Located Name -> Loc Var
nameToVar name = let (nm, qual) = destructName $ unLoc name
                     symb = Var { varqual = qual, varname = nm }
                 in Loc (getLoc name) symb

rdrnamesToVars :: [Located RdrName] -> [Loc Var]
rdrnamesToVars = map rdrnameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

rdrnameToVar :: Located RdrName -> Loc Var
rdrnameToVar name =
  let (nm, qual) = f $ unLoc name
      symb = Var { varqual = qual, varname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Qual mn occnm) = (GHC.occNameString occnm, Just $ GHC.moduleNameString mn)
    f (GHC.Orig m occnm) = (GHC.occNameString occnm, Just $ GHC.moduleNameString $ GHC.moduleName m)
    f (GHC.Exact nm) = destructName nm
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, Nothing)


rdrnameToDef :: Located RdrName -> Loc Def
rdrnameToDef name = 
  let (nm, _) = f $ unLoc name
      symb = Def { defname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, Nothing)
    f _ = error "Impossible: qualified name in a binding"

