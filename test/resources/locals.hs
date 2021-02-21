module Locals where


-- No Locals
test1 = 1 + 2

-- Locals: x
test2 x = x + test1

--- Locals: x, y, z, p
test3 x y = let z = 1; p = 2 in z + y + p + y

-- Locals: x
test4 x = test2 x

-- Locals: x, f, x, test3
test5 x = test3 $ f x
  where f x = let test3 = 20 in x + test3 + Locals.test4 x
