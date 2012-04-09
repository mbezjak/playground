-- 1.
(↑) :: Int -> Int -> Int
x ↑ 0 = 1
x ↑ e = x * x↑(e-1)

-- 2.
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length xs

-- length' [1,2,3]
-- 1 + length' [1,2]
-- 1 + 1 + length' [2]
-- 1 + 1 + 1 + length' []
-- 1 + 1 + 1 + 0
-- 3

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop (n-1) xs

-- drop' 3 [1,2,3,4,5]
-- drop' 2 [2,3,4,5]
-- drop' 1 [3,4,5]
-- drop' 0 [4,5]
-- [4,5]

init' ::  [a] -> [a]
init' [_]    = []
init' (x:xs) = x:init xs

-- init' [1,2,3]
-- init' 1:init' [2,3]
-- init' 1:2:init' [3]
-- init' 1:2:[]
-- [1,2]

-- 3.
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x:replicate' (n-1) x

(!!!) :: [a] -> Int -> a
(x:_) !!! 0  = x
(x:xs) !!! n = xs !!! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) | x == e    = True
               | otherwise = elem' e xs

-- 4.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

-- 5.
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = mergeHalves (halve xs)
  where mergeHalves (first, last) = merge (msort first) (msort last)

-- 6.
sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum xs

take' :: Integral b => b -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs
