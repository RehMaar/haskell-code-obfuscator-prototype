{-# LANGUAGE TypeFamilies, FlexibleInstances, TupleSections,
             OverloadedStrings, DeriveFunctor, RankNTypes
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
import Control.Monad
import Control.Monad.State
import Data.Foldable

import           OneLinePrinter
import           Utils
import           Source
import           Transform.Types
import           Transform.Query
import           Transform.Context
import           Transform.Rename

import           Debug.Trace

import Data.Generics

data ObfuscateContext
  = OC
  { oc_source_ctx:: SourceContext
  , oc_parsed_source :: ParsedSource
  , oc_symbols :: [Char]
  , oc_range_symbols :: (Int, Int)
  , oc_range_name_len :: (Int, Int)
  , oc_generator :: StdGen
  , oc_used_symbols :: [String]
  }

type Obfuscate a = State ObfuscateContext a

evalObfuscate f seed = evalState f . initObfuscate seed

initObfuscateCommon symbols range seed si = let
    sctx = initSC si
  in OC sctx (si_parsed_source si) symbols (0, pred $ length symbols) range (mkStdGen seed) []

initObfuscate = initObfuscateCommon defaultSymbols defaultRange
  where
    defaultSymbols = ['A'..'Z'] ++ ['a'..'z']
    defaultRange = (1, 10)

setSource :: ParsedSource -> Obfuscate ()
setSource src = do
  modify (\ctx -> ctx { oc_parsed_source = src })

getNextFreshVar :: Obfuscate String
getNextFreshVar = do
  name <- getNextFreshName
  case name of
    (n:ns) -> return (toLower n : ns)
    []     -> error "getNextFreshVar: empty name"

getNextFreshName :: Obfuscate String
getNextFreshName = do
   wordLen <- getWordLen
   name <- getName wordLen
   flag <- isUsed name
   if flag
   then getNextFreshName
   else return name
  where
    isUsed :: String -> Obfuscate Bool
    isUsed name = do
      used <- oc_used_symbols <$> get
      return $ name `elem` used


    getName :: Int -> Obfuscate String
    getName 0 = return ""
    getName wordLen = do
      symb <- getSymbol
      rest <- getName (pred wordLen)
      return (symb:rest)

    getSymbol = do
      ctx <- get
      idx <- getInt (oc_range_symbols ctx)
      return $ oc_symbols ctx !! idx


    getWordLen :: Obfuscate Int
    getWordLen = do
      range <- oc_range_name_len <$> get
      getInt range

    getInt :: (Int, Int) -> Obfuscate Int
    getInt range = do
      gen <- oc_generator <$> get
      let (int, gen') = randomR range gen
      modify (\ctx -> ctx { oc_generator = gen' })
      return int

applyTransformation :: Typeable a => (a -> a) -> Obfuscate ()
applyTransformation = applyTransformationCommon apply

applyTransformationCommon
  :: (Typeable a)
  => (forall a b . (Data b, Typeable a) => (a -> a) -> b -> b)
  -> (a -> a)
  -> Obfuscate ()
applyTransformationCommon applier f = do
  src <- oc_parsed_source <$> get
  modify (\ctx -> ctx { oc_parsed_source = applier f src })

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
transformDoToLam :: Obfuscate ()
transformDoToLam = do
  src <- oc_parsed_source <$> get
  src <- applyM transform src
  modify (\ctx -> ctx { oc_parsed_source = src })
 where
  transform :: HsExpr GhcPs -> Obfuscate (HsExpr GhcPs)
  transform (HsDo _ DoExpr (L _ es)) = fromJust <$> foldrM update Nothing es
  transform x = return x

  update :: ExprLStmt GhcPs -> Maybe (HsExpr GhcPs) -> Obfuscate (Maybe (HsExpr GhcPs))
  update (L _ stmt) (Just k) = Just <$> stmtToExpr stmt k
  update (L _ stmt) Nothing  = Just <$> lastStmt stmt

{-  foldToExpr :: [ExprLStmt GhcPs] -> HsExpr GhcPs
  foldToExpr [e     ] = lastStmt (unLoc e)
  foldToExpr (e : es) = stmtToExpr (unLoc e) (foldToExpr es)-}

  -- body >>= \pat -> k
  bind body pat k = SG.op body (fromString ">>=") (SG.lambda pat k)

  stmtToExpr :: ExprStmt GhcPs -> HsExpr GhcPs -> Obfuscate (HsExpr GhcPs)
  -- pat <- body ==> body >>= \pat -> {}
  stmtToExpr (BindStmt _ pat body _ _) k = return $ bind (unLoc body) [pat] k
  -- let pat = body ==> (\pat -> {} ) body
  -- let fun => (\funname -> {}) (let fun in funname)
  stmtToExpr (LetStmt _ (L _ lbs)    ) k = foldLets k lbs
  -- body => body >>= \_ -> {}
  stmtToExpr (BodyStmt _ body _ _    ) k = return $ bind (unLoc body) [SG.wildP] k

  stmtToExpr (LastStmt _ body _ _    ) k = error "What case?"

  lastStmt (LastStmt _ body _ _) = return $ unLoc body
  lastStmt (BodyStmt _ body _ _) = return $ unLoc body

  foldLets k (HsValBinds _ (ValBinds _ binds _)) =
    foldrM bindToLamOrLet k (map unLoc $ GHC.bagToList binds)

  -- Case: let funname = fun in funanme
  --      (\funname -> {}) (let fun in funname)
  -- bindToLamOrLet :: HsExpr GhcPs -> HsBind GhcPs -> Obfuscate (HsExpr GhcPs)
  bindToLamOrLet fb@FunBind { fun_id = L _ funname } k =
    return $
    let
      lbind =
        noLoc $ HsValBinds noExt $ ValBinds noExt (GHC.listToBag [noLoc fb]) []
      pat = VarPat noExt (noLoc funname)
    in
      SG.lambda [pat] k
        SG.@@ HsLet noExt lbind (noLoc $ HsVar noExt $ noLoc funname)
  -- Case: let (Ctr args) = body => (\(Ctr args) -> {}) (let newname = body in newname)
  --
  -- Problem:
  -- This example is a correct haskell code (but it loops):
  --   Just a | a > 0 = 10
  --          | otherwise = 20
  --
  -- But it translates into:
  -- (\(Just a) -> ..) (let <freshName> | a > 0 = 10 | otherwise = 20 in <freshName>)
  -- And this translation can't be compiled.
  --
  -- Just for now we (I) support only such behaviour.
  --
  bindToLamOrLet PatBind { pat_lhs = pat, pat_rhs = body } k = do
     freshName <- getNextFreshVar
     let funName = GHC.mkRdrUnqual $ GHC.mkVarOcc freshName :: RdrName
     let body' = noLoc $ createFunBind funName [] body :: LHsBindLR GhcPs GhcPs
     let bind = noLoc $ HsValBinds noExt $ ValBinds noExt (GHC.listToBag [body']) []
     let letBind = HsLet noExt bind (noLoc $ HsVar noExt $ noLoc funName)
     return $ SG.lambda [pat] k SG.@@ letBind

  createFunBind name pats grhss
    = FunBind
    { fun_ext = noExt
    , fun_id = noLoc name
    , fun_matches = createMatchGroup name pats grhss
    , fun_co_fn = WpHole
    , fun_tick = []}
  createMatchGroup name pats grhss
    = MG
    { mg_ext = noExt
    , mg_alts = noLoc [noLoc $ createMatch name pats grhss]
    , mg_origin = GHC.Generated }
  createMatch name pats grhss
    = Match
    { m_ext = noExt
    , m_ctxt = FunRhs { mc_fun = noLoc name, mc_fixity = GHC.Prefix, mc_strictness = NoSrcStrict}
    , m_pats = pats
    , m_grhss = grhss}

-- | Transform strings and chars.
--
-- "String" -> map toChar [ints], toChar a = toEnum a :: Char
--
-- Maybe, there's better solutions.
--
-- TODO: bug with overloaded literals
transformStringAndChars :: Obfuscate ()
transformStringAndChars = do
  ctx <- get
  let src = oc_parsed_source ctx
  freeNameToChar <- getNextFreshVar
  modify (\ctx -> ctx { oc_parsed_source = transform freeNameToChar src })
 where
  transform :: String -> ParsedSource -> ParsedSource
  transform freeName src = addIfChanged freeName $ do
    src <- applyButM stopTransformString (transformStringM freeName) src
    applyButM stopTransformString (transformCharM freeName) src

  addIfChanged freeName result
    | isChanged result
    = fmap (addDeclWithSig decl sig) $ fromChanged result
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
  help' x = L (getLoc x) $ HsPar noExt x
transformOpToApp x = x

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

generateRenamings :: Obfuscate [(String, String)]
generateRenamings = do
  sc <- oc_source_ctx <$> get
  let globals = sc_allow_rename_globals sc
  let locals  = (varname . lcelem) <$> sc_allow_rename_locals sc
  let rvs = unique $ sort $ globals ++ locals
  generateRenamings' rvs
  where
    generateRenamings' :: [String] -> Obfuscate [(String, String)]
    generateRenamings' [] = return []
    generateRenamings' (n:ns) = do
      name <- getNextFreshVar
      rs   <- generateRenamings' ns
      return ((n, name):rs)

obfuscateNames :: Obfuscate ParsedSource
obfuscateNames = do
  ctx <- get
  renamings <- generateRenamings
  let src1 = rename (oc_source_ctx ctx) renamings (oc_parsed_source ctx)
  -- let src2 = renameImportedSymbols (oc_source_ctx ctx) renamings src1
  setSource src1
  return src1

obfuscateStructure :: Obfuscate ParsedSource
obfuscateStructure = do
  transformStringAndChars
  transformDoToLam
  applyTransformation addParens
  applyTransformationCommon applyTopDown transformOpToApp
  applyTransformation transformIfCase
  applyTransformation transformMultiArgLam
  oc_parsed_source <$> get

obfuscate = obfuscateWithSeed 0

obfuscateWithSeed :: Int -> SourceInfo -> ParsedSource
obfuscateWithSeed = evalObfuscate obfuscate''
  where
    obfuscate'' = do
      obfuscateNames
      obfuscateStructure
