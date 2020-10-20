{-# LANGUAGE TupleSections, RankNTypes #-}
module Transform.Query (
    -- Queries to collect some info into a list.
    collect, collectBut, collectBut',
    -- Queries to apply changes to a data structre.
    apply, applyBut, applyTopDown, applyM, applyButM,
    -- A monad to track if changes were applied.
    Changed, isChanged, fromChanged, unchanged, changed
    ) where
    
import Data.Generics as SYB
import Data.Monoid
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

applyButM :: (Data a, Monad m, Typeable b) =>
    GenericQ Bool ->
    (b -> m b) ->
     a -> m a
applyButM stopper changer = everywhereButM stopper (SYB.mkM changer)
  where
    everywhereButM :: forall m. Monad m => GenericQ Bool -> GenericM m -> GenericM m
    everywhereButM q f x
         | q x = pure x
         | otherwise = do { x' <- gmapM (everywhereButM q f) x; f x'}

type Changed a = (Any, a)

isChanged (Any o, _) = o
fromChanged (_, a) = a

changed = (Any True, )
unchanged = (Any False, )
