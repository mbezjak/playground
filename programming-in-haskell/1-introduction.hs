-- 1.
quadruple x = x * 4

-- 2.
-- sum [x] = x
-- x + sum [] = x
-- x + 0 = x
-- x = x
-- QED

-- 3.
product' [] = 1
product' (x:xs) = x * product xs

-- 4.
qsort' [] = []
qsort' (x:xs) = larger ++ [x] ++ smaller
  where smaller = [a | a <- xs, a <= x]
        larger  = [a | a <- xs, a > x]

-- 5.
qsort'' [] = []
qsort'' (x:xs) = smaller ++ [x] ++ larger
  where smaller = [a | a <- xs, a < x]
        larger  = [a | a <- xs, a > x]
