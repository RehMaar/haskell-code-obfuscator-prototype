module OneLinePrinter
  (oneline, showOneLine)
  where

import qualified Util as GHC.Util
import GHC
import ConLike
import Bag (bagToList)
import GHC.Paths (libdir)
import DynFlags

import qualified Outputable as Out
import Data.String
import Data.List
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map

import qualified Language.Haskell.GHC.ExactPrint as EP
import qualified Language.Haskell.GHC.ExactPrint.Types as EP

import Outputable as O

import Utils

separate :: SDoc -> [SDoc] -> [SDoc]
separate _ [] = []
separate symb (x:xs) = x O.<> symb : separate symb xs


onelineHead Nothing _ = text ""
onelineHead (Just name) exports =
    pmodname name <+>
    pexports exports
  where
    pmodname name = text "module" <+> ppr name

    pexports :: Maybe (Located [LIE GhcPs]) -> SDoc
    pexports Nothing = text "where"
    pexports (Just es) = 
      lparen
      <+> hcat (punctuate comma (map ppr (unLoc es)))
      <+> text ") where"

onelineImport :: [LImportDecl GhcPs] -> SDoc
onelineImport = hsep . separate semi . map ppr

onelineDecls :: [LHsDecl GhcPs] -> SDoc
onelineDecls = hsep . separate semi  . map (ol_decl . unLoc)
  where
    ol_decl :: HsDecl GhcPs -> SDoc
    ol_decl (ValD _ bind) = ol_bind bind
    ol_decl s@(SigD _ _) = ppr s
    -- TODO: Take them from annotations?
    ol_decl (WarningD{}) = text ""
    ol_decl x = error "Unsupported type of decl"

    ol_bind :: HsBind GhcPs -> SDoc
    ol_bind (FunBind _ _ mg _ _) = ol_mg  mg
    ol_bind (PatBind _ pat grhss _) = ol_pat pat <+> ol_grhss equals grhss
    ol_bind _ = error "Unsupported type of bind"

    ol_mg :: MatchGroup GhcPs (LHsExpr GhcPs) -> SDoc
    -- onelineMatchGroup = undefined
    ol_mg (MG _ ms _) = hsep $ punctuate semi (map (ol_match . unLoc) $ unLoc ms)

    ol_match :: Match GhcPs (LHsExpr GhcPs) -> SDoc
    ol_match (Match _ ctx pat grhss) =
      let (opat, con) = ol_fun ctx pat
      in opat <+> ol_grhss con grhss

    -- TODO: Strictness
    ol_fun :: HsMatchContext RdrName-> [Pat GhcPs] -> (SDoc, SDoc)
    ol_fun (FunRhs fn fixity _) pat | fixity == Prefix = (ol_prefix pat (unLoc fn), equals)
                                    | fixity == Infix  = (ol_infix  pat (unLoc fn), equals)
    ol_fun CaseAlt (p:_) = (ol_pat p, arrow)
    ol_fun LambdaExpr p = (char '\\' <+> ol_pats p, arrow)
    ol_fun x _ = error $ showSDocUnsafe $ ppr x

    ol_grhss con (GRHSs _ gs lbs) =
      hcat (map ((\(a, b) -> a <+> con <+> b) . ol_grhs . unLoc) gs)
      O.<> if isEmptyLocalBindsPR (unLoc lbs)
           then empty
           else text " where {" <+> ol_lbinds (unLoc lbs) <+> text "}"

    ol_lbinds (HsValBinds _ b) = ol_valbind b
    ol_lbinds (HsIPBinds _ b)  = error "HsIPBinds?"
    ol_lbinds _ = text ""

    ol_valbind (ValBinds _ binds sigs) = hsep $ punctuate semi $ map snd (sort_by_loc decls)
      where
        decls :: [(SrcSpan, SDoc)]
        decls = [(loc, ppr sig)  | L loc sig <- sigs] ++
                [(loc, ol_bind bind) | L loc bind <- bagToList binds]

        sort_by_loc decls = sortBy (comparing fst) decls

    ol_grhs :: GRHS GhcPs (LHsExpr GhcPs) -> (SDoc, SDoc)
    ol_grhs (GRHS _ guards body) = (ol_guards guards, ol_expr (unLoc body))

    ol_guards :: [GuardLStmt GhcPs] -> SDoc
    ol_guards [] = empty
    ol_guards gs = vbar <+> hsep (punctuate comma (map (ol_stmt . unLoc) gs))

    ol_stmt :: Stmt GhcPs (LHsExpr GhcPs) -> SDoc
    ol_stmt (BodyStmt _ expr _ _) = ol_expr $ unLoc expr
    ol_stmt (LetStmt _ lb) = text "let" <+> ol_lbinds (unLoc lb)
    ol_stmt (BindStmt _ pat body _ _) = ol_pat (unLoc pat) <+> larrow <+> ol_expr (unLoc body)
    ol_stmt x = error $ "Unhandled stmt: " ++ showElem x

    ol_prefix :: [LPat GhcPs] -> RdrName -> SDoc
    ol_prefix pat fn = pprPrefixOcc fn <+> ol_pats pat

    ol_infix :: [LPat GhcPs] -> RdrName -> SDoc
    ol_infix [p1, p2]   fn = ol_pat p1 <+> pprInfixOcc fn <+> ol_pat p2
    ol_infix (p1:p2:ps) fn = (lparen <+> ol_infix [p1, p2] fn <+> rparen) <+> ol_pats ps

    ol_pats ps = hsep (map ppr ps)
    ol_pat = ppr

    ol_expr :: HsExpr GhcPs -> SDoc
    ol_expr (HsPar _ e) = lparen O.<> ol_expr (unLoc e) O.<> rparen
    ol_expr (HsLam _ mg) = ol_mg mg
    ol_expr (HsApp _ f arg) = ol_expr (unLoc f) <+> ol_expr (unLoc arg)
    ol_expr (OpApp _ l op r)
      | Just pp_op <- ppr_infix_expr (unLoc op)
      = infixly pp_op
      | otherwise
      = prefixly
      where
        prefixly = ol_expr (unLoc op) <+> ol_expr (unLoc l) <+> ol_expr (unLoc r)
        infixly op = ol_expr (unLoc l) <+> op <+> ol_expr (unLoc r)
    ol_expr (HsCase _ expr mg) =
        text "case"
        <+> ol_expr (unLoc expr)
        <+> text "of {"
        <+> ol_mg mg
        <+> text "}"
    ol_expr (HsIf _ _ e1 e2 e3) =
        text "if"
        <+> ol_expr (unLoc e1)
        <+> text "then"
        <+> ol_expr (unLoc e2)
        <+> text "else"
        <+> ol_expr (unLoc e3)
    ol_expr (HsLet _ lbinds (L _ stmts)) =
       text "let"
       <+> ol_lbinds (unLoc lbinds)
       <+> text "in"
       <+> ol_expr stmts
    ol_expr (HsDo _ stmt_ctx expr_stmt) =
        text "do {"
        <+> ol_expr_do stmt_ctx (unLoc expr_stmt)
        <+> text "}"
    ol_expr (ExplicitList _ _ es) = lbrack O.<> hsep (punctuate comma (map (ol_expr . unLoc) es)) O.<> rbrack
    -- ol_expr (ExplicitTuple _ arg box) = error "ExplicitTuple: TODO"
    -- ol_expr (HsOverLit _ (OverLit _ lit wit)) = ppr lit <+> text "{\n" <+> ol_expr wit <+> text "}\n"
    ol_expr e = pprExpr e

    ol_expr_do :: HsStmtContext Name -> [LStmt GhcPs (LHsExpr GhcPs)] -> SDoc
    ol_expr_do DoExpr stmts = hsep (punctuate semi (map (ol_stmt . unLoc) stmts))
    ol_expr_do ListComp stmts = ol_expr_list_comp stmts
    ol_expr_do x _ = error $ "HsDo: " ++ showSDocUnsafe (ppr x)

    ol_expr_list_comp stmts
      | Just (quals, L _ (LastStmt _ body _ _)) <- GHC.Util.snocView stmts
      = lbrack <+> ol_expr (unLoc body) <+> ol_guards quals <+> rbrack
      | otherwise
      = error "ListComp: badly formed"

    -- Copied from GHC.Hs.Expr
    ppr_infix_expr :: HsExpr GhcPs -> Maybe SDoc
    ppr_infix_expr (HsVar _ (L _ v))    = Just (pprInfixOcc v)
    ppr_infix_expr (HsConLikeOut _ c)   = Just (pprInfixOcc (conLikeName c))
    ppr_infix_expr (HsRecFld _ f)       = Just (pprInfixOcc f)
    ppr_infix_expr (HsUnboundVar _ h@TrueExprHole{}) = Just (pprInfixOcc (unboundVarOcc h))
    ppr_infix_expr (HsWrap _ _ e)       = ppr_infix_expr e
    ppr_infix_expr _                    = Nothing

-- | Prints _only_ header comments.
processHeaderComments ans
  | Just val <- lookupWith partial $ Map.toList ans
  = let coms = map (EP.commentContents . fromComment) $ filter isComments $ map fst $ EP.annsDP val
    in hcat (map text coms)
  | otherwise
  = text ""
  where
    annConName = EP.CN "HsModule"
    partial (EP.AnnKey _ m) = m == annConName

    lookupWith _ [] = Nothing
    lookupWith f ((k, v):xs)
      | f k = Just v
      | otherwise = lookupWith f xs

    isComments (EP.AnnComment _) = True
    isComments _ = False

    fromComment (EP.AnnComment c) = c
    fromComment _ = error "fromComment: not a comment"

oneline :: EP.Anns -> HsModule GhcPs -> SDoc
oneline ans (HsModule mname exports imports decls _ _) =
    processHeaderComments ans
    <+> onelineHead   mname exports
    <+> onelineImport imports
    <+> onelineDecls  decls

showOneLine dynFlags ans mod = showSDocOneLine dynFlags (oneline ans mod)
