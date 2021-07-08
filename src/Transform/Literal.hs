{-# LANGUAGE OverloadedStrings #-}
module Transform.Literal where

import           GHC
import           BasicTypes
import           Transform.Internal.Types
import           Transform.Internal.Query
import           Transform.Internal.Context
import           Transform.Internal.Generate
import           GHC.SourceGen                 as SG
import           GHC.SourceGen.Binds           as SG
import qualified FastString                    as GHC
import           Data.Generics                 as SYB
import Data.String
import Utils
import Control.Monad.State
import Data.Char
import Control.Arrow
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Transform strings and chars.
--
-- "String" -> map toChar [ints], toChar a = toEnum a :: Char
--
-- Maybe, there's better solutions.
--
-- TODO: bug with overloaded literals
-- TODO: decide when use WithChr and when WithEnum
transformStringAndChars :: Transform ()
transformStringAndChars = do
  ctx <- get
  let src = tc_parsed_source ctx
  freeNameToChar <- getNextFreshVar
  let transform' = transformWithChr freeNameToChar
  modify (\ctx -> ctx { tc_parsed_source = transform' src })
 where
  -- use `varName` as variable and declaration
  transformWithEnum varName =
    transform
      (transformWithEnumHelperDecl varName)
      (transformCharWithEnum varName)
      (transformStringWithEnum varName)

  -- use `freeName` as Module name
  transformWithChr freeName =
    transform
      (transformWithChrHelperImport name)
      (transformCharWithChr name')
      (transformStringWithChr name')
    where
      name = let (n:ns) = freeName in toUpper n : ns
      name' = name <> ".chr"

  -- transform :: String -> ParsedSource -> ParsedSource
  transform helper transformString transformChar src = addIfChanged helper $ do
    src <- applyButM stopTransformString transformString src
    applyButM stopTransformString transformChar src

  addIfChanged helper result
    | isChanged result
    = helper <$> fromChanged result
    | otherwise
    = fromChanged result

  -- This approach doesn't work as intended for some reason!
  -- transformString :: Data a => a -> Changed a
  -- transformString = return `extM`
  --                   transformCharM `extM`
  --                   transformStringM freeName

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
  listToHsList = listToHsList' id

  listToHsList' f = SG.list . map (f . SG.int . toInteger)

  stringToList ""       = []
  stringToList (x : xs) = fromEnum x : stringToList xs

  --------------------------------------------------------------
  -- Transformation using Data.Char.chr
  --------------------------------------------------------------

  -- 'c' -> Data.Char.chr n
  transformCharWithChr :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformCharWithChr name (HsLit _ (HsChar _ chr)) =
    changed $ createVar name  SG.@@ SG.int (toInteger $ fromEnum chr)
  transformCharWithChr _ x = unchanged x

  -- "str" -> [Data.Char.chr n, Data.Char.chr n]
  transformStringWithChr :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformStringWithChr name e@(HsLit _ (HsString _ str)) =
    changed $ listToHsList' f $ stringToList (GHC.unpackFS str)
    where f = (createVar name SG.@@)
  transformStringWithChr _ x = unchanged x

  -- add import qualified Data.Char as FreeName (chr)
  transformWithChrHelperImport name = addImport im
    where im = qualified' $ import' "Data.Char" `as'` name' `exposing` [var "chr"]
          name' = fromString name

  --------------------------------------------------------------
  -- Transformation using adding a declaration and Prelude.map
  --------------------------------------------------------------
  -- 'c' -> (toChar n)
  transformCharWithEnum :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformCharWithEnum freeName (HsLit _ (HsChar _ chr)) =
    changed $ createVar freeName SG.@@ SG.int (toInteger $ fromEnum chr)
  transformCharWithEnum _ x = unchanged x

  --
  -- TODO:
  --  1. `map` may be hidden!
  --  2. Need to obfuscate `map` also!
  --
  -- Example:
  --   "str" -> [chr1, chr2, chr3]
  --         -> (map toChar [int1, int2, int3])
  transformStringWithEnum :: String -> HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformStringWithEnum freeName e@(HsLit _ (HsString _ str)) =
    let lst  = listToHsList $ stringToList (GHC.unpackFS str)
        fn   = SG.var (fromString "map") SG.@@ SG.var (fromString freeName)
    in  changed $ SG.par $ fn SG.@@ lst
  transformStringWithEnum _ x = unchanged x

  --
  -- toChar :: Int -> Char
  -- toChar = toEnum
  --
  transformWithEnumHelperDecl freeName = addDeclWithSig decl sig
    where
      decl = createDecl freeName "toEnum"
      sig = SG.typeSig (fromString freeName) (createVar "Int" SG.--> createVar "Char")

--
-- 1. First simple solution:
--    123 -> (i.d.t)z
--    z = 0; t = n + 3; d = n + 2*10; i = n + 100
--
--
-- TODO: shuffle declarations
transformIntegrals :: Transform ()
transformIntegrals = do
  src <- getSource
  src <- evalStateT (do { src <- applyM transform src; addDigitsSigs; pure src}) Map.empty
  setSource src
  where
    transform :: HsExpr' -> StateT (Map.Map Integer String) (State TransformContext) HsExpr'
    transform (HsOverLit _ (OverLit { ol_val = HsIntegral IL {..}})) = do
      let digits = numberToDigits il_value
      names <- forM digits digitToFun
      zeroName <- getZeroDecl
      pure $ createComposition zeroName names
    transform x = pure x

    getZeroDecl :: StateT (Map.Map Integer String) (State TransformContext) String
    getZeroDecl = do
      mZeroDecl <- gets (Map.lookup 0)
      case mZeroDecl of
        Just d -> pure d
        Nothing -> do
          zeroName <- lift $ getNextFreshVar
          let zeroDecl = createZeroDecl zeroName
          lift $ addNewDecl zeroDecl
          addDigitDecl 0 zeroName
          pure zeroName

    -- -- Add to map: Map Int {- Digit -} HsBind' {- Corresponding definition -}
    -- Generate fun `i n r = n * 10 + d`, n -- a number, d -- digit
    -- digitToFun :: Integer -> Transform (String, HsDecl')
    digitToFun :: Integer -> StateT (Map.Map Integer String) (State TransformContext) String
    digitToFun d = do
      mZeroDecl <- gets (Map.lookup d)
      case mZeroDecl of
        Just d -> pure d
        Nothing -> do
          fun_name <- lift $ getNextFreshVar
          arg_name <- lift $ getNextFreshVar -- getNextFreshLocalVar
          let decl = createDigitFun fun_name arg_name d
          lift $ addNewDecl decl
          addDigitDecl d fun_name
          pure fun_name

    numberToDigits = unfoldr f where
      f 0 = Nothing
      f n = Just $ (snd &&& fst) $ divMod n 10

    createDigitFun :: String -> String -> Integer -> HsDecl'
    createDigitFun funName argName digit =
      funBind (fromString funName) $ match [pvar_ argName] $
        op (SG.int digit) "+" (op (var_ argName) "*" (op (SG.int 10))

    createZeroDecl :: String -> HsDecl'
    createZeroDecl funName =
      funBind (fromString funName) $ match [] $ SG.int 0

    createComposition :: String -> [String] -> HsExpr'
    createComposition z [] = var_ z
    createComposition z (d:ds) =
     let num = foldl (\acc digit -> op acc  "." (var_ digit)) (var_ d) ds
     in par (num SG.@@ var_ z)

    addDigitDecl :: Integer -> String -> StateT (Map.Map Integer String) (State TransformContext) ()
    addDigitDecl digit name = modify (Map.insert digit name)

    addDigitsSigs :: StateT (Map.Map Integer String) (State TransformContext) ()
    addDigitsSigs = do
       mp <- get
       let zeroName = fromMaybe (error "transforming integrals: no zero definition") $ mp Map.!? 0
       let mp' = Map.delete 0 mp
       let names = Map.elems mp'
       let sigDecl = createSigDecl names
       let zeroDecl = createZeroSigDecl zeroName
       lift $ addNewDecl sigDecl
       lift $ addNewDecl zeroDecl

    createSigDecl :: [String] -> HsDecl'
    createSigDecl names =
      typeSigs (fromString <$> names) $ [var "Num" @@ var "a"] ==> var "a" --> var "a"

    createZeroSigDecl :: String -> HsDecl'
    createZeroSigDecl name =
      typeSig (fromString name) $ [var "Num" @@ var "a"] ==> var "a"
