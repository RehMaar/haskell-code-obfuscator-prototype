module Transform.Literal where

import           GHC
import           Transform.Internal.Types
import           Transform.Internal.Query
import           Transform.Internal.Context
import           GHC.SourceGen                 as SG
import           GHC.SourceGen.Binds           as SG
import qualified FastString                    as GHC
import           Data.Generics                 as SYB
import Data.String
import Utils
import Control.Monad.State

-- | Transform strings and chars.
--
-- "String" -> map toChar [ints], toChar a = toEnum a :: Char
--
-- Maybe, there's better solutions.
--
-- TODO: bug with overloaded literals
transformStringAndChars :: Transform ()
transformStringAndChars = do
  ctx <- get
  let src = tc_parsed_source ctx
  freeNameToChar <- getNextFreshVar
  modify (\ctx -> ctx { tc_parsed_source = transform freeNameToChar src })
 where
  transform :: String -> ParsedSource -> ParsedSource
  transform freeName src = addIfChanged freeName $ do
    src <- applyButM stopTransformString (transformStringM freeName) src
    applyButM stopTransformString (transformCharM freeName) src

  addIfChanged freeName result
    | isChanged result
    = addDeclWithSig decl sig <$> fromChanged result
    | otherwise
    = fromChanged result
    where
      decl = createDecl freeName "toEnum"
      sig = SG.typeSig (fromString freeName) (createVar "Int" SG.--> createVar "Char")

  -- This approach doesn't work as intended for some reason!
  -- transformString :: Data a => a -> Changed a
  -- transformString = return `extM`
  --                   transformCharM `extM`
  --                   transformStringM freeName

  -- 'c' -> (toChar n)
  transformCharM :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformCharM freeName (HsLit _ (HsChar _ chr)) =
    changed $ createVar freeName SG.@@ SG.int (toInteger $ fromEnum chr)
  transformCharM _ x = unchanged x

  --
  -- TODO:
  --  1. `map` may be hidden!
  --  2. Need to obfuscate `map` also!
  --
  -- Example:
  --   "str" -> [chr1, chr2, chr3]
  --         -> (map toChar [int1, int2, int3])
  transformStringM :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformStringM freeName e@(HsLit _ (HsString _ str)) =
    let lst  = listToHsList $ stringToList (GHC.unpackFS str)
        fn   = SG.var (fromString "map") SG.@@ SG.var (fromString freeName)
    in  changed $ SG.par $ fn SG.@@ lst
  transformStringM _ x = unchanged x

  stopTransformString :: GenericQ Bool
  stopTransformString = const False `extQ`
                        stopTransformStringOverLit `extQ`
                        stopTransformStringSyntaxExpr

  stopTransformStringSyntaxExpr  :: SyntaxExpr GhcPs -> Bool
  stopTransformStringSyntaxExpr _ = True

  stopTransformStringOverLit :: HsOverLit GhcPs -> Bool
  stopTransformStringOverLit OverLit{} = True
  stopTransformStringOverLit _         = False

  listToHsList :: [Int] -> HsExpr GhcPs
  listToHsList = SG.list . map (SG.int . toInteger)

  stringToList ""       = []
  stringToList (x : xs) = fromEnum x : stringToList xs
