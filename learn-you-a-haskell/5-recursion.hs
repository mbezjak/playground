maximum' :: Ord a => [a] -> a
maximum' []         = error "Maximum of empty list"
maximum' [x]        = x
maximum' (x1:x2:xs) = if x1 > x2 then maximum' (x1:xs) else maximum' (x2:xs)

maximum'' :: Ord a => [a] -> a
maximum'' []  = error "Maximum of empty list"
maximum'' [x] = x
maximum'' (x1:x2:xs)
  | x1 > x2   = maximum'' (x1:xs)
  | otherwise = maximum'' (x2:xs)

maximum''' :: Ord a => [a] -> a
maximum''' []  = error "Maximum of empty list"
maximum''' [x] = x
maximum''' (x:xs)
  | x > maxTail = x
  | otherwise   = maxTail
  where maxTail = maximum''' xs

maximum4 = foldr max 0

maximum5 [] = error "Empty list"
maximum5 [x] = x
maximum5 (x:xs) = max x (maximum5 xs)

replicate' n e
  | n <= 0    = []
  | otherwise = [e] ++ replicate' (n-1) e

replicate'' :: (Num i, Ord i) => i -> a -> [a]
replicate'' n x
  | n <= 0    = []
  | otherwise = x:replicate'' (n-1) x

take' :: (Integral i, Ord i) => i -> [a] -> [a]
take' _ []    = []
take' n (x:xs)
  | n <= 0    = []
  | otherwise = x : take' (n-1) xs

take2 :: Int -> [a] -> [a]
take2  _ []   = []
take2 n (x:xs)
  | n <= 0    = []
  | otherwise = x : take2 (n-1) xs

take3 :: (Num i, Ord i) => i -> [a] -> [a]
take3 n _ | n <= 0 = []
take3 _ []         = []
take3 n (x:xs)     = x : take3 (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ []    = False
elem' e (x:xs)
  | e == x    = True
  | otherwise = e `elem'` xs

elem2 e xs = any (==e) xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
  let smallerSorted = qsort [a | a <- xs, a <= x]
      biggerSorted  = qsort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted

qsort2 :: Ord a => [a] -> [a]
qsort2 [] = []
qsort2 (x:xs) = smaller ++ [x] ++ bigger
  where smaller = qsort2 [a | a <- xs, a <= x]
        bigger  = qsort2 [a | a <- xs, a > x]

unique :: Eq a => [a] -> [a]
unique []     = []
unique (x:xs) = x : unique eliminated
  where eliminated = [a | a <- xs, a /= x]

unique2 :: Eq a => [a] -> [a]
unique2 []     = []
unique2 (x:xs) = x : unique2 (filter (/=x) xs)
