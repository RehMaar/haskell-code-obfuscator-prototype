module Transform.Internal.Generate where

import GHC
import GHC.SourceGen
import BasicTypes
import Data.String
import TcEvidence

import Utils

var_ :: String -> HsExpr GhcPs
var_ = var . fromString

funBind_ name pats grhss
  = FunBind
  { fun_ext = noExt
  , fun_id = noLoc name
  , fun_matches = matchGroup__ (funCtx_ name) pats grhss
  , fun_co_fn = WpHole
  , fun_tick = []
  }

matchGroup_ ctx matches
  = MG
  { mg_ext = noExt
  , mg_alts = noLoc matches
  , mg_origin = Generated
  }

matchGroup__ ctx pats grhss =
  matchGroup_ ctx [noLoc $ match_ ctx pats grhss]

match_ ctx pats grhss
  = Match
  { m_ext = noExt
  , m_ctxt = ctx
  , m_pats = pats
  , m_grhss = grhss
  }

grhss_ grhss localBinds = GRHSs
  { grhssExt = noExt
  , grhssGRHSs = grhss
  , grhssLocalBinds = localBinds
  }

grhss__ g = grhss_ g $  noLoc $ EmptyLocalBinds noExt

grhs_ stmt expr = GRHS noExt stmt $ noLoc expr

funCtx_ name = FunRhs { mc_fun = noLoc name, mc_fixity = GHC.Prefix, mc_strictness = NoSrcStrict}
caseCtx_ = CaseAlt
