import Data.Char

-- 1.
comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p xs = [f x | x <- xs, p x]

comprehension' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension' f p = map f . filter p

-- 2.
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- 3.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10*x + y) 0

-- 5.
-- sumsqreven = compose [sum, map (^2), filter even]
-- function types are
--   [ Num a => [a] -> a
--   , Num a => [a] -> [a]
--   , Integral a => [a] -> [a]
--   ]

-- 6.
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x -> \y -> f (x, y)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x,y) -> f x y

-- 7.
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f

-- 8 & 9.
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin = unfold (==0) (`mod` 2) (`div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

parity :: [Bit] -> Bit
parity = (`mod` 2) . length . filter odd

extend :: [Bit] -> [Bit]
extend bits = bits ++ [parity bits]

consume :: [Bit] -> [Bit]
consume bits = if correct then take 8 bits else error "Transmission error"
  where correct = parity (take 8 bits) == last bits

encode :: String -> [Bit]
encode = concat . map (extend . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int . consume) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail
