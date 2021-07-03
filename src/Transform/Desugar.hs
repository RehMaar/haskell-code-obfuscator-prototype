{-# LANGUAGE TypeFamilies, FlexibleInstances, OverloadedStrings, RankNTypes #-}
module Transform.Desugar where

import GHC
import Transform.Internal.Context
import Transform.Internal.Types
import Transform.Internal.Query

import GHC.SourceGen                 as SG
import GHC.SourceGen.Binds           as SG

import Control.Monad.State
import Data.Maybe
import Data.Foldable
import           Data.String
import Control.Monad
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
--
-- TODO:
--   * can use (>>)
--   * test case: <pat> <- <expr>
--     like `True <- return (x == y)`
transformDoToLam :: Transform ()
transformDoToLam = do
  src <- gets tc_parsed_source
  src <- applyM transform src
  modify (\ctx -> ctx { tc_parsed_source = src })
 where
  transform :: HsExpr GhcPs -> Transform (HsExpr GhcPs)
  transform (HsDo _ DoExpr (L _ es)) = fromJust <$> foldrM update Nothing es
  transform x = return x

  update :: ExprLStmt GhcPs -> Maybe (HsExpr GhcPs) -> Transform (Maybe (HsExpr GhcPs))
  update (L _ stmt) (Just k) = Just <$> stmtToExpr stmt k
  update (L _ stmt) Nothing  = Just <$> lastStmt stmt

  -- body >>= \pat -> k
  bind body pats k = SG.op body (fromString ">>=") (createLambda pats k)

  stmtToExpr :: ExprStmt GhcPs -> HsExpr GhcPs -> Transform (HsExpr GhcPs)
  -- pat <- body ==> body >>= \pat -> {}
  stmtToExpr (BindStmt _ pat@(XPat (L _ VarPat{})) body _ _) k = return $ bind (unLoc body) [pat] k
  -- complex pat <- body
  --   ==>
  --     \_ -> body >>= \r -> case r of { True -> {l}; _ -> fail "" }
  --
  -- TODO:
  --   Maybe, need add some exceptions, like for `Ctr a b c` (Constructor and VarPats inside)
  --   Because some problems with fail (and MonadFail?) may occur.
  stmtToExpr (BindStmt _ pat body _ _) k = do
    freshName <- getNextFreshVar
    let caseBody = case' (var $ fromString freshName) [match [pat] k, match [SG.wildP] (var "fail" SG.@@ SG.string "")]

    let freshRdrName = GHC.mkRdrUnqual $ GHC.mkVarOcc freshName :: RdrName
    let freshPat = VarPat noExt (noLoc freshRdrName)
    return $ bind (unLoc body) [freshPat] caseBody

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
  -- bindToLamOrLet :: HsExpr GhcPs -> HsBind GhcPs -> Transform (HsExpr GhcPs)
  bindToLamOrLet fb@FunBind { fun_id = L _ funname } k =
    return $
    let
      lbind =
        noLoc $ HsValBinds noExt $ ValBinds noExt (GHC.listToBag [noLoc fb]) []
      pat = VarPat noExt (noLoc funname)
    in
      createLambda [pat] k
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
     return $ createLambda [pat] k SG.@@ letBind

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

  -- Create lambda, add parens in patterns when needed
  createLambda :: [Pat'] -> HsExpr GhcPs -> HsExpr GhcPs
  createLambda pats body = SG.lambda (patParens <$> pats) body
    where
      patParens x@(XPat y) = XPat (patParens <$> y)
      patParens x@ParPat{} = x
      patParens x = ParPat noExt x


-- | Transform if-expression to case.
transformIfCase :: HsExpr GhcPs -> HsExpr GhcPs
transformIfCase (HsIf _ _ (L _ conde) (L _ ife) (L _ thene)) =
  case' conde [match [conP "True" []] ife, match [conP "False" []] thene]
transformIfCase x = x

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

addParens :: HsExpr GhcPs -> HsExpr GhcPs
addParens e@OpApp{}  = HsPar noExt (noLoc e)
addParens e@NegApp{} = HsPar noExt (noLoc e)
addParens x          = x
