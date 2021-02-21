module Types (B, D(..), F(MkF, f), E(..)) where

type A b = [(Int, Int, Int, Int,Int, Int, Int, Int, Int, Int, Int, b)]

data B = MkB { b_a :: Int, b_b :: Int }

data D = MkD { d_a :: Int, d_b :: Int }

data F = MkF { f :: Int } | MkF' { f' :: Bool } | MkF'' { f'' :: Bool }

data C a
  = MkC
  { c_a :: a
  , c_b :: Int
  }

newtype G a = MkG { g_a :: a }

test :: C Int -> Int
test c = c_a c

ctr :: C Int
ctr = MkC 0 1

class E a where
  fmap :: a -> Bool
  fmap _ = False


instance E B where
  fmap _ = True
