# Patterns

#### Most common:
* `WildPat (XWildPat p)`
   Wildcards: `f _ = undefined`
* `VarPat (XVarPat p) (Located (IdP p))`
   Simple variables: `f a = undefined`.
   `Located (IdP p)` in the first phase contains variable name.
* `AsPat (XAsPat p) (Located (IdP p)) (LPat p)`
   Patterns with asription: `f x@Nothing = undefined`
   `Located (IdP p)` is a name used in the ascription: `x`.
   `LPat p` is a pattern: `Nothing`. 
* `ParPat (XParPat p) (LPat p)`
  Just parenthesis `f (Just x) = undefined`
* `ListPat (XListPat p) [LPat p]`
  List in patterns: `f [1, 2, 3] = undefined`
* `TuplePat (XTuplePat p) [LPat p] Boxity`
  Tuples in patterns: `f (1, 2) = undefined`
  `Boxity` shows whether the turple is `Boxed` (by default) or `Unboxed` (with [`UnboxedTuples` extension](https://downloads.haskell.org/~ghc/8.8.4/docs/html/users_guide/glasgow_exts.html#unboxed-tuples))
* `NPat (XNPat p) (Located (HsOverLit p)) (Maybe (SyntaxExpr p)) (SyntaxExpr p)`
   Numbers in patterns: `f 1 = undefind; g 1.2 = undefined`
   TODO: why `SyntaxExpr`?
* `LitPat (XLitPat p) (HsLit p)`
   Literals in patterns: `f "a" = _`
   By some reason in my examples only strings are parsed in `LitPat`.
* `LazyPat (XLazyPat p) (LPat p)`
   Lazy patterns: `f ~(a, b) = undefined`
   Note: a lazy pattern match is translated to calling corresponding record field accessors.
* `ConPatIn (Located (IdP p)) (HsConPatDetails p)`
   Constructors at patterns: `f Nothing = undefined`
* `ConPatOut {}` -- ??
* `CoPat {}` -- ???

#### Unique

* `XPat (XXPat p)` is "Trees that Grow extension point for new constructors".
  Earlier every pattern I notice is always wrapped in this constructor,
  so the pattern in `f _` isn't just `WildPat{}`, it is `XPat (L _ WildPat{})`.
  But now, in 8.10.5, is not like that anymore.

#### With extensions

* `SigPat {}` -- `ScopedTypeVariables`
* `BangPat  {}` -- `BangPatterns`
* `SumPat {}` -- `AnonymousSums 
* `ViewPat {}` -- `ViewPatterns`
* `NPlusKPat {}` -- `NPlusKPat`
* `SplicePat {}` -- `TemplateHaskell`
