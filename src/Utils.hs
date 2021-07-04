{-# LANGUAGE TypeFamilies #-}
module Utils where
    
import qualified GHC.SourceGen as SG
import qualified GHC.SourceGen.Binds as SG

import GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified Name as GHC

import Data.List
import Data.String
import Data.Generics as SYB
import Control.Arrow ((&&&), first)

import System.IO (hPutStrLn, stderr)

import Outputable as Out

noExt :: NoExtField
noExt = noExtField

-- Printing for debug purposes
showElem :: Out.Outputable p => p -> String
showElem = Out.showSDocUnsafe . Out.ppr

showL xs = intercalate "\n" (map show xs)

putStrLnErr = hPutStrLn stderr

unique :: Ord a => [a] -> [a]
unique = fmap head . group . sort

getModuleName :: GHC.HsModule GHC.GhcPs -> Maybe String
getModuleName = fmap (GHC.moduleNameString . GHC.unLoc)  . GHC.hsmodName

-- | Destruct Name into a name and its qualifier.
destructName :: GHC.Name -> (String, Maybe String)
destructName = first GHC.occNameString . destructNameToOcc

destructNameToOcc :: GHC.Name -> (GHC.OccName, Maybe String)
destructNameToOcc name =
  let qual = GHC.moduleNameString . GHC.moduleName <$> GHC.nameModule_maybe (GHC.unLoc name)
      nm = GHC.nameOccName $ GHC.unLoc name
  in (nm, qual)

destructRdrName :: GHC.RdrName -> (GHC.OccName, Maybe String)
destructRdrName (GHC.Unqual n)  = (n, Nothing)
destructRdrName (GHC.Qual mn n) = (n, Just $ GHC.moduleNameString mn)
destructRdrName (GHC.Orig m n)  = (n, Just $ GHC.moduleNameString $ GHC.moduleName m)
destructRdrName (GHC.Exact n)   = (GHC.nameOccName n, GHC.moduleNameString . GHC.moduleName <$> GHC.nameModule_maybe n)

lookupGen _ _ [] _ = Nothing
lookupGen f p (x:xs) y
  | f x y = Just (p x)
  | otherwise = lookupGen f p xs y

-- TODO: maybe there are better places for these functions?
createDecl :: String -> String -> HsDecl GhcPs
createDecl name call = SG.funBind (fromString name) $ SG.match [] $ SG.var $ fromString call

createVar name = SG.var (fromString name)

addDecl :: HsDecl GhcPs -> HsModule GhcPs  -> HsModule GhcPs
addDecl d (HsModule n e i ds x y) = HsModule n e i (ds ++ [noLoc d]) x y

addDeclWithSig :: HsDecl GhcPs -> HsDecl GhcPs -> HsModule GhcPs  -> HsModule GhcPs
addDeclWithSig d s (HsModule n e i ds x y) = HsModule n e i (ds ++ [noLoc s, noLoc d]) x y

addImport :: ImportDecl GhcPs -> HsModule GhcPs -> HsModule GhcPs
addImport im hsmod@HsModule { hsmodImports = imports } = hsmod { hsmodImports = imports ++ [noLoc im] }

-- Debug things
showPat WildPat  {} = "WildPat  "
showPat VarPat {} = "VarPat"
showPat LazyPat  {} = "LazyPat  "
showPat AsPat {} = "AsPat"
showPat ParPat {} = "ParPat"
showPat BangPat  {} = "BangPat  "
showPat ListPat  {} = "ListPat  "
showPat TuplePat {} = "TuplePat"
showPat SumPat {} = "SumPat"
showPat ConPatIn {} = "ConPatIn"
showPat ConPatOut {} = "ConPatOut"
showPat ViewPat {} = "ViewPat"
showPat SplicePat {} = "SplicePat"
showPat LitPat {} = "LitPat"
showPat NPat    {} = "NPat    "
showPat NPlusKPat {} = "NPlusKPat"
showPat SigPat {} = "SigPat"
showPat CoPat {} = "CoPat"
showPat pat@(XPat {})
 | XPat (L _ p) <- pat = "XPat: " Prelude.<> showPat p
