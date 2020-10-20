{-# LANGUAGE TypeFamilies, FlexibleInstances, TupleSections,
             OverloadedStrings, DeriveFunctor
#-}

module Transform.Obfuscate where

import           Language.Haskell.GHC.ExactPrint
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Parsers
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Utils
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Delta
                                               as EP

import           GHC.SourceGen                 as SG
import           GHC.SourceGen.Binds           as SG

import           GHC
import qualified OccName                       as GHC
import qualified RdrName                       as GHC
import qualified SrcLoc                        as GHC
import qualified Bag                           as GHC
import qualified Name                          as GHC
import qualified Module                        as GHC
import qualified FastString                    as GHC
import qualified BasicTypes                    as GHC
import qualified HscTypes                      as GHC
import qualified TcRnTypes                     as GHC
import qualified UniqDFM                       as GHC
import           GHC.Paths                      ( libdir )
import           DynFlags
import           TcEvidence                     ( HsWrapper(WpHole) )
import qualified Outputable                    as Out


import           Data.Generics                 as SYB
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.List
import           Data.String
import           Data.Char
import           Control.Arrow                  ( (&&&), first )
import           System.Random

import           OneLinePrinter
import           Utils
import           Source
import           Transform.Types
import           Transform.Query
import           Transform.Context
import           Transform.Rename

import           Debug.Trace

import Data.Generics

generateObfuscatedNamesOld ns = zip ns $ generateObfuscatedNames' 1 ns
  where
    generateObfuscatedNames' :: Int -> [String] -> [String]
    generateObfuscatedNames' n = zipWith (\n name -> gen n name) [n ..]
      where
        -- gen n _ = concatMap show $ take n [n..]
            gen n _ = 'a' : show n -- take n ['a', 'a' ..]

generateObfuscatedNamesRandom :: StdGen -> [Char] -> [String] -> (StdGen, [(String, String)])
generateObfuscatedNamesRandom gen pull names =
  fst <$>
  foldr (\n (gen, (rs, names)) ->
           let (gen1, newName) = generateFreshName pull gen names
           in (gen1, ((n, newName) : rs, newName : names)))
        (gen, ([], names))
        names

pullOfSymbols :: [Char]
pullOfSymbols = ['A'..'Z'] ++ ['a'..'z']
-- range = (0, pred $ length pullOfSymbols)
-- rangeWordLen = (1, 10 :: Int)

generateName :: [Char] -> StdGen -> Int -> (StdGen, String)
generateName _ gen 0 = (gen, [])
generateName pull gen wordLen =
  let (symbolNo, gen') = randomR range gen
  in ((pullOfSymbols !! symbolNo):) <$> generateName pull gen' (pred wordLen)
  where range = (0, pred $ length pull)

tryName names newName = newName `notElem` names

generateFreshName :: [Char] -> StdGen -> [String] -> (StdGen, String)
generateFreshName pull gen names =
  let (wordLen, gen1) = randomR (1, 10 :: Int) gen
      (gen2, (s:newName')) = generateName pull gen1 wordLen
      newName = toLower s : newName'
  in if tryName names newName
     then (gen2, newName)
     else generateFreshName pull gen2 names


-- | Transform operators to application form.
--
-- Example: x + y -> (+) x y
--
-- TODO: need correct locations?
transformOpToApp :: HsExpr GhcPs -> HsExpr GhcPs
transformOpToApp e@(OpApp _ left op right) = app
  (noLoc $ par $ noLoc $ app op $ noLoc $ par left)
  (noLoc $ par right)
 where
  app = HsApp noExt

  par = HsPar noExt

  help' :: LHsExpr GhcPs -> LHsExpr GhcPs
  -- help' e@(L _ (HsPar{})) = e
  -- help' e@(L _ (HsVar{})) = e
  -- help' e@(L _ (HsLit{})) = e
  -- help' e@(L _ (HsOverLit{})) = e
  help' x = L (getLoc x) $ HsPar noExt x
transformOpToApp x = x

-- | Transform do-notation into lambda form.
--
-- Example 1:
--  do { r1 <- a2; r2 <- a3; ..; e }
--   ==>
--  (a1 >>= \r1 -> a3 >>= \r2 -> ... ; \rn -> e)
-- 
-- Example 2:
--   do { r1 <- a1; let l1 = s1; r2 <- a2; ... e }
--    ==>
--   a1 >>= \r1 -> (\l1 -> a2 >>= \r2 -> (... \rn -> e)) s1
transformDoToLam :: HsExpr GhcPs -> HsExpr GhcPs
transformDoToLam (HsDo _ DoExpr (L _ es)) = foldToExpr es
 where
  foldToExpr :: [ExprLStmt GhcPs] -> HsExpr GhcPs
  foldToExpr [e     ] = lastStmt (unLoc e)
  foldToExpr (e : es) = stmtToExpr (unLoc e) (foldToExpr es)

  -- body >>= \pat -> k
  bind body pat k = SG.op body (fromString ">>=") (SG.lambda pat k)

  stmtToExpr :: ExprStmt GhcPs -> HsExpr GhcPs -> HsExpr GhcPs
  -- pat <- body ==> body >>= \pat -> {}
  stmtToExpr (BindStmt _ pat body _ _) k = bind (unLoc body) [pat] k
  -- let pat = body ==> (\pat -> {} ) body
  -- let fun => (\funname -> {}) (let fun in funname)
  stmtToExpr (LetStmt _ (L _ lbs)    ) k = foldLets k lbs
  -- body => body >>= \_ -> {}
  stmtToExpr (BodyStmt _ body _ _    ) k = bind (unLoc body) [SG.wildP] k

  stmtToExpr (LastStmt _ body _ _    ) k = error "What case?"

  lastStmt (LastStmt _ body _ _) = unLoc body
  lastStmt (BodyStmt _ body _ _) = unLoc body

  foldLets k (HsValBinds _ (ValBinds _ binds _)) =
    foldLets' k (map unLoc $ GHC.bagToList binds)

  foldLets' k []       = k
  foldLets' k (b : bs) = foldLets' (bindToLamOrLet k b) bs

  -- Case: let funname = fun in funanme
  bindToLamOrLet :: HsExpr GhcPs -> HsBind GhcPs -> HsExpr GhcPs
  bindToLamOrLet k fb@FunBind { fun_id = L _ funname } =
    let
      lbind =
        noLoc $ HsValBinds noExt $ ValBinds noExt (GHC.listToBag [noLoc fb]) []
      pat = VarPat noExt (noLoc funname)
    in
      SG.lambda [pat] k
        SG.@@ HsLet noExt lbind (noLoc $ HsVar noExt $ noLoc funname)
  bindToLamOrLet k PatBind { pat_lhs = pat } = error "Pat"
transformDoToLam x = x

-- | Transform strings and chars.
--
-- "String" -> map toChar [ints], toChar a = toEnum a :: Char
--
-- Maybe, there's better solutions.
--
-- TODO: bug with overloaded literals
transformStringAndChars
  :: String -> Located (HsModule GhcPs) -> Located (HsModule GhcPs)
transformStringAndChars freeName src =
  addIfChanged $ do
  src <- applyButM stopTransformString (transformStringM freeName) src
  applyButM stopTransformString transformCharM src
 where
  addIfChanged result
    | isChanged result
    = fmap (addDeclWithSig decl sig) $ fromChanged result
    | otherwise
    = fromChanged result
  decl = createDecl freeName "toEnum"
  sig = SG.typeSig (fromString freeName) (createVar "Int" SG.--> createVar "Char")

  -- This approach doesn't work as intended for some reason!
  -- transformString :: Data a => a -> Changed a
  -- transformString = return `extM`
  --                   transformCharM `extM`
  --                   transformStringM freeName

  -- 'c' -> (toChar n)
  transformCharM :: HsExpr GhcPs -> Changed (HsExpr GhcPs)
  transformCharM (HsLit _ (HsChar _ chr)) =
    changed $ createVar freeName SG.@@ SG.int (toInteger $ fromEnum chr)
  transformCharM x = unchanged x

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

-- | Transform if-expression to case.
transformIfCase :: HsExpr GhcPs -> HsExpr GhcPs
transformIfCase (HsIf _ _ (L _ conde) (L _ ife) (L _ thene)) =
  case' conde $ [match [conP ("True") []] ife, match [conP ("False") []] thene]
transformIfCase x = x

-- | Transform multi-argument lambda to nested lambdas
transformMultiArgLam :: HsExpr GhcPs -> HsExpr GhcPs
transformMultiArgLam (HsLam _ mg)
  | MG { mg_alts = L _ [L _ match] } <- mg
  , Match { m_ctxt = LambdaExpr, m_pats = pats, m_grhss = gs } <- match
  , GRHSs { grhssGRHSs = [L _ g] } <- gs
  , GRHS _ _ (L _ expr) <- g
  = foldr (\pat expr -> lambda [pat] expr) expr pats
transformMultiArgLam x = x

addParens :: HsExpr GhcPs -> HsExpr GhcPs
addParens e@OpApp{}  = HsPar noExt (noLoc e)
addParens e@NegApp{} = HsPar noExt (noLoc e)
addParens x          = x

-- For debug pursposes
instance Show (GenLocated SrcSpan RdrName) where
  show (L s r) = rdrName2String r

obfuscateNames :: SourceInfo -> (Anns, ParsedSource)
obfuscateNames (SourceInfo ans src rvs _) =
  let mod       = unLoc src
      modName   = fromMaybe "Main" $ getModuleName mod

      ctx        = initTransformContext modName rvs $ unLoc src
      gen        = mkStdGen 13
      (gen2, renamings) = generateObfuscatedNamesRandom gen pullOfSymbols (tcNames ctx)
      -- renamings = generateObfuscatedNamesOld (tcNames ctx)
      (ans1, src1) = rename ctx renamings ans src
      (ans2, src2) = renameImportedSymbols ctx renamings ans src1
  in (ans2, src2)

obfuscateStructure :: SourceInfo -> (Anns, ParsedSource)
obfuscateStructure (SourceInfo ans src rvs _) =
    (ans,) $
    apply addParens $
    -- transformStringAndChars "toChar" $
    apply transformDoToLam           $
    applyTopDown transformOpToApp $
    apply transformIfCase $
    apply transformMultiArgLam $
    src

obfuscate :: SourceInfo -> (Anns, ParsedSource)
obfuscate (SourceInfo ans src rvs _) =
  let mod       = unLoc src
      modName   = fromMaybe "Main" $ getModuleName mod

      -- TODO: maybe some transformations will need transformation context
      ctx        = initTransformContext modName rvs $ unLoc src
      gen        = mkStdGen 13
      (gen2, renamings) = generateObfuscatedNamesRandom gen pullOfSymbols (tcNames ctx)
      -- renamings = generateObfuscatedNamesOld (tcNames ctx)
      -- Rename
      (ans1, src1) = rename ctx renamings ans src
      (ans2, src2) = renameImportedSymbols ctx renamings ans src1
      -- Possibly, add new declaration.
      (gen3, newDeclName) = generateFreshName pullOfSymbols gen2 (uncurry (++) $ unzip renamings)
      src3                = transformStringAndChars newDeclName src2
    in
    (ans2,) $
    apply addParens                  $
    apply transformDoToLam           $
    applyTopDown transformOpToApp $
    apply transformIfCase $
    apply transformMultiArgLam $
    src3
