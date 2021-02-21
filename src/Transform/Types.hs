{-# LANGUAGE DeriveFunctor #-}
module Transform.Types where

import GHC
import SrcLoc as GHC
import qualified OccName as GHC
import qualified RdrName as GHC

import Data.Maybe (maybe)

import Utils

data Qual a
  = NoQual
  | PQual a
  -- ^ A qualification is presented in parsed source (parsed qual).
  | RQual a
  -- ^ A qualification is determened at renaming stage (renamed qual).
  deriving (Show, Eq, Ord)

isNoQual NoQual = True
isNoQual _ = False

isQual (PQual _) = True
isQual _ = False

getQual (PQual q) = Just q
getQual _ = Nothing

getRealQual (PQual q) = q
getRealQual (RQual q) = q

sameQualMod :: String -> Qual String -> Bool
sameQualMod _ NoQual = False
sameQualMod name q = name == getRealQual q

data Var = Var { varname :: String, varqual :: Qual String }
  deriving (Ord, Eq)

instance Show Var where
  show (Var n NoQual) = show n
  show (Var n (PQual q)) = show (q ++ "." ++ n)
  show (Var n (RQual q)) = show (q ++ "#" ++ n)

mkVar = Var

mkVarNoQual name = Var name NoQual

mkVarQual name qual = Var name (PQual qual)

mkVarRQual name qual = Var name (RQual qual)

isVarQualified (Var _ (PQual _)) = True
isVarQualified _ = False

getVarQual (Var _ (PQual q)) = q

getVarName (Var n _) = n

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
  deriving (Eq, Functor, Show, Ord)

-- instance Show a => Show (Loc a) where
--   show (Loc l e) = "Loc {} " ++ show e

toLoc a = Loc (getLoc a) (unLoc a)

namesToVars :: [Located Name] -> [Loc Var]
namesToVars = map nameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

nameToVar :: Located Name -> Loc Var
nameToVar name = Loc (getLoc name) $ nameToVar' $ unLoc name

nameToVar' :: Name -> Var
nameToVar' name = let (nm, qual) = destructName name
                  in mkVar nm (maybe NoQual PQual qual)

rdrnamesToVars :: [Located RdrName] -> [Loc Var]
rdrnamesToVars = map rdrnameToVar . filter (GHC.isGoodSrcSpan . GHC.getLoc)

rdrnameToVar :: Located RdrName -> Loc Var
rdrnameToVar name =
  let (nm, qual) = f $ unLoc name
      symb = mkVar nm qual -- Var { varqual = qual, varname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Qual mn occnm) = (GHC.occNameString occnm, PQual $ GHC.moduleNameString mn)
    f (GHC.Orig m occnm) = (GHC.occNameString occnm,  PQual $ GHC.moduleNameString $ GHC.moduleName m)
    f (GHC.Exact nm) = let (name, qual) = destructName nm in (name, maybe NoQual PQual qual)
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, NoQual)


rdrnameToDef :: Located RdrName -> Loc Def
rdrnameToDef name = 
  let (nm, _) = f $ unLoc name
      symb = Def { defname = nm }
  in Loc (getLoc name) symb
  where
    f (GHC.Unqual occnm) = (GHC.occNameString occnm, Nothing)
    f _ = error "Impossible: qualified name in a binding"

