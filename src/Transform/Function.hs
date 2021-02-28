module Transform.Function where

import GHC
import           GHC.SourceGen                 as SG
import           GHC.SourceGen.Binds           as SG

-- | Transform multi-argument lambda to nested lambdas
transformMultiArgLam :: HsExpr GhcPs -> HsExpr GhcPs
transformMultiArgLam (HsLam _ mg)
  | MG { mg_alts = L _ [L _ match] } <- mg
  , Match { m_ctxt = LambdaExpr, m_pats = pats, m_grhss = gs } <- match
  , GRHSs { grhssGRHSs = [L _ g] } <- gs
  , GRHS _ _ (L _ expr) <- g
  = foldr (\pat expr -> lambda [pat] expr) expr pats
transformMultiArgLam x = x
