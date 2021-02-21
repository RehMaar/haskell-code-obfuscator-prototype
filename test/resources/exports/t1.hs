module T1(test2) where

class B a where
  bmap :: a -> Bool

test1 = 2

test2 :: (Show a, B a) => a -> IO ()
test2 a = putStrLn $ show $ bmap a

instance Show (IO a) where
    show _ = "<<IO a>>"

instance B Bool where
  bmap False = True
  bmap _ = False
