module Utils where

import qualified GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified Name as GHC

import Data.List
import Data.Generics as SYB
import Control.Arrow ((&&&), first)

import Outputable as Out

unique :: Ord a => [a] -> [a]
unique = fmap head . group . sort

putList :: Show a => [a] -> IO ()
putList = putStrLn . intercalate "\n" . map show

getModuleName :: GHC.HsModule GHC.GhcPs -> Maybe String
getModuleName = fmap (GHC.moduleNameString . GHC.unLoc)  . GHC.hsmodName

showElem :: Out.Outputable p => p -> String
showElem = Out.showSDocUnsafe . Out.ppr

-- | Collect information into a list.
collect f = SYB.everything (++) ([] `SYB.mkQ` f)

-- | Collect information into a list until some condition is met.
collectBut' f = SYB.everythingBut (++) (([], False) `SYB.mkQ` f)

-- | Like `collectBut'` but with explicit condition given.
collectBut f p = collectBut' (f &&& p)

-- | Destruct Name into a name and its qualifier.
destructName :: GHC.Name -> (String, Maybe String)
destructName = first GHC.occNameString . destructNameToOcc

destructNameToOcc :: GHC.Name -> (GHC.OccName, Maybe String)
destructNameToOcc name =
  let qual = (GHC.moduleNameString . GHC.moduleName) <$> GHC.nameModule_maybe (GHC.unLoc name)
      nm = GHC.nameOccName $ GHC.unLoc name
  in (nm, qual)

destructRdrName :: GHC.RdrName -> (GHC.OccName, Maybe String)
destructRdrName (GHC.Unqual n)  = (n, Nothing)
destructRdrName (GHC.Qual mn n) = (n, Just $ GHC.moduleNameString mn)
destructRdrName (GHC.Orig m n)  = (n, Just $ GHC.moduleNameString $ GHC.moduleName m)
destructRdrName (GHC.Exact n)   = (GHC.nameOccName n, (GHC.moduleNameString . GHC.moduleName) <$> GHC.nameModule_maybe n)
