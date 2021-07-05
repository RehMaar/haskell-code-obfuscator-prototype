{-# LANGUAGE TypeFamilies, FlexibleInstances, OverloadedStrings, RankNTypes #-}
module Transform.Desugar
  ( transformDoToLam
  , transformIfCase
  , transformArgsToLam
  , transformMultiArgLam
  , transformOpToApp
  , addParens
  )
  where
    
import GHC
import Transform.Internal.Context
import Transform.Internal.Types
import Transform.Internal.Query
import Transform.Internal.Generate

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

import Utils
import Data.Functor

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
  stmtToExpr (BindStmt _ pat@(L _ VarPat{}) body _ _) k = return $ bind (unLoc body) [pat] k
  -- complex pat <- body
  --   ==>
  --     \_ -> body >>= \r -> case r of { True -> {l}; _ -> fail "" }
  --
  -- TODO:
  --   Maybe, need add some exceptions, like for `Ctr a b c` (Constructor and VarPats inside)
  --   Because some problems with fail (and MonadFail?) may occur.
  stmtToExpr (BindStmt _ pat' body _ _) k = do
    let pat = unLoc pat'
    freshName <- getNextFreshVar
    let caseBody = case' (var $ fromString freshName) [match [pat] k, match [SG.wildP] (var "fail" SG.@@ SG.string "")]

    let freshRdrName = GHC.mkRdrUnqual $ GHC.mkVarOcc freshName :: RdrName
    let freshPat = noLoc $ VarPat noExt (noLoc freshRdrName)
    return $ bind (unLoc body) [freshPat] caseBody

  -- let pat = body ==> (\pat -> {} ) body
  -- let fun => (\funname -> {}) (let fun in funname)
  stmtToExpr (LetStmt _ (L _ lbs)    ) k = foldLets k lbs
  -- body => body >>= \_ -> {}
  stmtToExpr (BodyStmt _ body _ _    ) k = return $ bind (unLoc body) [noLoc SG.wildP] k

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
      pat = noLoc $ VarPat noExt (noLoc funname)
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
     let body' = noLoc $ funBind_ funName [] body :: LHsBindLR GhcPs GhcPs
     let bind = noLoc $ HsValBinds noExt $ ValBinds noExt (GHC.listToBag [body']) []
     let letBind = HsLet noExt bind (noLoc $ HsVar noExt $ noLoc funName)
     return $ createLambda [pat] k SG.@@ letBind

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


-- | Transform multi-argument lambda to nested lambdas
transformMultiArgLam :: HsExpr GhcPs -> HsExpr GhcPs
transformMultiArgLam (HsLam _ mg)
  | MG { mg_alts = L _ [L _ match] } <- mg
  , Match { m_ctxt = LambdaExpr, m_pats = pats, m_grhss = gs } <- match
  , GRHSs { grhssGRHSs = [L _ g] } <- gs
  , GRHS _ _ (L _ expr) <- g
  = foldr (\(L _ pat) expr -> lambda [pat] expr) expr pats
transformMultiArgLam x = x


--
-- 1. f x y = undefined => f = \x y -> undefined
-- 2. f 3 4 = undefined => f = \x y -> case (x, y) of { (3, 4) undefined }
-- 3.
--   f [] = undefined
--   f (x:xs) = undefined
--   =>
--   f = \xs -> case xs of { ... }
--
-- Algo:
-- 1. Find patterns in all cases
-- 2. For each position create new varaible
transformArgsToLam :: Transform ()
transformArgsToLam = do
  src <- getSource
  src <- applyM transform src
  setSource src
  where
    hasArguments _ = True

    transform :: HsBind GhcPs -> Transform (HsBind GhcPs)
    transform f@FunBind{..}
      | hasArguments f = do
        let name = unLoc fun_id
        new_matches <- newMatches name fun_matches
        pure $ f { fun_matches = new_matches }
    transform x = pure x

    newMatches :: RdrName -> MatchGroup GhcPs (LHsExpr GhcPs) -> Transform (MatchGroup GhcPs (LHsExpr GhcPs))
    newMatches name mg@MG {..} = do
      new_alts <- newAlts name $ (unLoc mg_alts)
      pure $ mg { mg_alts = noLoc [noLoc new_alts] }

    newAlts :: RdrName -> [LMatch GhcPs (LHsExpr GhcPs)] -> Transform (Match GhcPs (LHsExpr GhcPs))
    newAlts name alts = do
      let arguments = argPats alts
      vars <- replicateM (length $ fst $ head arguments) getNextFreshVar
      let pats = map (noLoc . VarPat noExt . noLoc . GHC.mkVarUnqual . GHC.mkFastString) vars
      pure $ match_ (funCtx_ name) pats $ body_ vars arguments
      where
        -- [strings] -> [(pats, grhss)] ->
        body_ :: [String] -> [([LPat GhcPs], GRHSs GhcPs (LHsExpr GhcPs))] -> GRHSs GhcPs (LHsExpr GhcPs)
        body_ vars args = grhss__ [noLoc grhs]
          where
            -- case (x1, x2, ..) of { (pat1, pat2, ..) -> body }
            -- case tuples of matchGroup
            grhs :: GRHS GhcPs (LHsExpr GhcPs)
            grhs =
              grhs_ [] $ HsCase noExt (tuples vars) $
                           matchGroup_ caseCtx_ matches
              where
                 matches :: [LMatch GhcPs (LHsExpr GhcPs)]
                 matches = args <&> \(pats, body) ->
                             noLoc $ match_ caseCtx_ (tuples' pats) body

            tuples :: [String] -> LHsExpr GhcPs
            tuples [x] = noLoc $ var_ x
            tuples xs = noLoc $ tuple (var_ <$> xs)

            tuples' :: [LPat GhcPs] -> [LPat GhcPs]
            tuples' [x] = [x]
            tuples' xs = [noLoc $ tuple $ unLoc <$> xs]

    argPats :: [LMatch GhcPs (LHsExpr GhcPs)] -> [([LPat GhcPs], GRHSs GhcPs (LHsExpr GhcPs))]
    argPats = map (argPats' . unLoc)
    argPats' (Match _ _ pats body) = (pats, body)

    patToCase newName cases = undefined

-- Create lambda, add parens in patterns when needed
createLambda :: [LPat GhcPs] -> HsExpr GhcPs -> HsExpr GhcPs
createLambda pats body = SG.lambda (patParens <$> pats) body
  where
    -- patParens x@(XPat y) = XPat (patParens <$> y)
    patParens :: LPat GhcPs -> Pat GhcPs
    patParens (L _ x@ParPat{}) = x
    patParens x = ParPat noExt x
