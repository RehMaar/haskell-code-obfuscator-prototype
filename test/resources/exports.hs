module Exports
  ( B         -- data without ctrs or fields
              -- Local symbols: MkB, b_a, b_b
  , D(..)     -- data with all ctrs and fields
  , F(MkF, f) -- data with a ctr and a field
              -- Local symbols: MkF', f', MkF'', f''
  , E(..)     -- class with all funs (and default impl)
  , J(..)     -- class with all funs (and no default impl)
  , H         -- class without any fun
              -- Local symbols: hmap
  , test2     -- a fun
              -- Local funs: ctr, test1
  , module Data.List -- just to test
  ) where

import Data.List

data B = MkB { b_a :: Int, b_b :: Int }

data D = MkD { d_a :: Int, d_b :: Int } | MkD' { d_a' :: Int }

data F = MkF { f :: Int } | MkF' { f' :: Bool } | MkF'' { f'' :: Bool }

test1 :: B -> Int
test1 c = b_a c

test2 :: Int
test2 = 1 + 2

test3 :: D -> Int
test3 d = d_a d

ctr :: B
ctr = MkB 0 1

class E a where
  fmap :: a -> Bool
  fmap _ = False

instance E B where
  fmap _ = True

class H a where
  hmap :: a -> Bool

instance H B  where
  hmap _ = False

class J a where
  jmap :: a -> Bool

instance Show B where
  show _ = "<<B>>"

class Jj a where
  jjmap :: a -> Bool
