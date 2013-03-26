goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList a b = [ goldbach x | x <- [a..b], even x ]

goldbachList' :: Int -> Int -> Int -> [(Int,Int)]
goldbachList' a b l = [ (y,x) | (x,y) <- goldbachList a b, x>l, y>l ]

goldbach :: Int -> (Int, Int)
goldbach n = head [ (a, n-a) | a <- primesR 2 n, isPrime (n-a) ]

primesR :: Int -> Int -> [Int]
primesR a b = [ x | x <- [a..b], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..limit]
  where limit = floor . sqrt . fromIntegral $ x
