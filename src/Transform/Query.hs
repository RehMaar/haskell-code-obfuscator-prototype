{-# LANGUAGE DeriveFunctor #-}
module Transform.Query (
    -- Queries to collect some info into a list.
    collect, collectBut, collectBut',
    -- Queries to apply changes to a data structre.
    apply, applyBut, applyTopDown, applyM,
    -- A monad to track if changes were applied.
    Changed, isChanged, fromChanged, unchanged, changed
    ) where
    
import Data.Generics as SYB
import Control.Arrow ((&&&))

-- | Collect information into a list.
collect f = SYB.everything (++) ([] `SYB.mkQ` f)

-- | Collect information into a list until some condition is met.
collectBut' f = SYB.everythingBut (++) (([], False) `SYB.mkQ` f)

-- | Like `collectBut'` but with explicit condition given.
collectBut f p = collectBut' (f &&& p)

-- | Apply a change to a parsed tree.
apply changer = SYB.everywhere (SYB.mkT changer)

-- | Apply a change to a parsed tree with a stop condition.
applyBut stopper changer = SYB.everywhereBut (SYB.mkQ False stopper) (SYB.mkT changer)

-- | Apply a change to a parsed tree in top-down manner.
applyTopDown changer = SYB.everywhere' (SYB.mkT changer)

applyM changer = SYB.everywhereM (SYB.mkM changer)

data Changed a = Changed { isChanged :: Bool, fromChanged :: a }
  deriving Functor

instance Applicative Changed where
    pure = Changed False
    Changed b1 f <*> Changed b2 a = Changed (b1 || b2) (f a)

instance Monad Changed where
   Changed b a >>= f = case f a of { Changed b2 a -> Changed (b || b2) a }

changed = Changed True
unchanged = Changed False
