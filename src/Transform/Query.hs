module Transform.Query where

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
