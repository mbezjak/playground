import Data.Char

-- 1.
sumsquares = sum [x^2 | x <- [1..100]]

-- 2.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 3.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], pyth x y z]
  where pyth x y z = x^2 + y^2 == z^2

-- 4.
factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Integral a => a -> [a]
perfects n = [x | x <- [1..n], perfect x]
  where perfect x = sum (init (factors x)) == x

-- 5.
twogenerators = [(x,y) | x <- [1,2,3], y <- [4,5,6]]
onegenerator  = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]


-- 6.
find' :: Eq a => a -> [(a,b)] -> [b]
find' k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find' x (zip xs [0..])

-- 7.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 8.
lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

startChar :: Bool -> Char
startChar False = 'a'
startChar True  = 'A'

let2int :: Char -> Char -> Int
let2int c s = ord c - ord s

int2let :: Int -> Char -> Char
int2let n s = chr (ord s + n)

shift :: Int -> Char -> Char
shift n c | isLower c = shiftWithStartChar (startChar False)
          | isUpper c = shiftWithStartChar (startChar True)
          | otherwise = c
  where shiftWithStartChar s = int2let ((let2int c s + n) `mod` 26) s

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4,
         6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.1, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (realToFrac n / realToFrac m) * 100

freqs :: String -> [Float]
freqs xs = [percent (count x nocases) n | x <- ['a'..'z']]
  where nocases = map toLower xs
        n       = lowers nocases

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs
