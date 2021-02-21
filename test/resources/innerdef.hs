module InnerDef where


test1 x = let r a = a + 1
  in f x
  where
     f x = j x
     j y = p y + 2
       where
         p y = y + 3
