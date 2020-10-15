{-# LANGUAGE TupleSections #-}
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

newtype Or = Or Bool deriving (Show)

instance Semigroup Or where
    (Or a) <> (Or b) = Or $ a || b

instance Monoid Or where
    mempty = Or False

type Changed a = (Or, a)

isChanged (Or o, _) = o
fromChanged (_, a) = a

changed = (Or True, )
unchanged = (Or False, )
