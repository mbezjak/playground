multThree :: Num a => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100

divideByFive :: Floating a => a -> a
divideByFive = (/5)

isUpperAlpha :: Char -> Bool
isUpperAlpha = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice' :: (a -> a) -> a -> a
applyTwice' f = f . f

starts :: Eq a => [a] -> [a] -> Bool
starts _ [] = False
starts [] _ = True
starts (x:xs) (y:ys) = x==y && starts xs ys

removePrefix ('f':'o':'o':xs) = xs
removePrefix xs = xs

removePrefix' xs = if starts "foo" xs then drop 3 xs else xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \b -> \a -> f a b

flip2 :: (a -> b -> c) -> (b -> a -> c)
flip2 f = g
  where g x y = f y x

flip3 :: (a -> b -> c) -> (b -> a -> c)
flip3 f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x -> \y -> f x : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs) = if p x then x: filter p xs else filter p xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 _ [] = []
filter2 p (x:xs)
  | p x = x : filter2 p xs
  | otherwise = filter2 p xs

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 _ [] = []
filter3 p (x:xs)
  | p x = x : rest
  | otherwise = rest
  where rest = filter3 p xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

largestDivisable :: Integral a => a
largestDivisable = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x: takeWhile' p xs
  | otherwise = []

chain :: Integral a => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd  n = n : chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

timesFn = map (*) [1..]
applyTimesFn = take 100 (map (\x -> x 10) timesFn)


-- lambdas

numLongChains2 :: Int
numLongChains2 = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

addThree' :: Num a => a -> a -> a -> a
addThree' = \x -> \y -> \z -> x + y + z

lamflip :: (a -> b -> c) -> (b -> a -> c)
lamflip f = \x y -> f y x


-- folds

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc []     = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

fsum :: Num a => [a] -> a
fsum = foldl (+) 0

fsum2 :: Num a => [a] -> a
fsum2 = foldl (\acc x -> acc + x) 0

felem :: Eq a => a -> [a] -> Bool
felem e = foldl (\acc x -> acc || x == e) False

felem2 :: Eq a => a -> [a] -> Bool
felem2 e = foldl (\acc x -> if x == e then True else acc) False

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc []     = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

foldmap :: (a -> b) -> [a] -> [b]
foldmap f = foldr (\x acc -> f x : acc) []

foldlmap :: (a -> b) -> [a] -> [b]
foldlmap f = foldl (\acc x -> acc ++ [f x]) []

fsum3 :: Num a => [a] -> a
fsum3 = foldr1 (+)

fmaximum :: Ord a => [a] -> a
fmaximum = foldr1 (\x acc -> if x > acc then x else acc)

freverse :: [a] -> [a]
freverse = foldr (\x acc -> acc ++ [x]) []

freverse2 :: [a] -> [a]
freverse2 = foldl (\acc x -> x : acc) []

fproduct :: Num a => [a] -> a
fproduct = foldr1 (*)

ffilter :: (a -> Bool) -> [a] -> [a]
ffilter p = foldr (\x acc -> if p x then x:acc else acc) []

fhead :: [a] -> a
fhead = foldr1 (\x _ -> x)

flast :: [a] -> a
flast = foldl1 (\_ x -> x)

freverse3 :: [a] ->[a]
freverse3 = foldl (flip (:)) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1


-- function application

fninlist  = map ($ 3) [(4+), (10*), (^2), sqrt]
fninlist2 = map (\f -> f 3) [(4+), (10*), (^2), sqrt]


-- function composition

fncomp x = ceiling (negate (tan (cos (max 50 x))))
fncomp2  = ceiling . negate . tan . cos . max 50

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum2 :: Integer
oddSquareSum2 = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum3 :: Integer
oddSquareSum3 =
  let oddSquares = filter odd $ map (^2) [1..]
      belowLimit = takeWhile (<10000) oddSquares
  in sum belowLimit
