goldbach :: Int -> [Int]
goldbach n = head . filter eq . combinations . primesR 1 $ n
  where eq xs = sum xs == n

goldbach' :: Int -> (Int, Int)
goldbach' n = head [ (a, n-a) | a <- primesR 2 n, isPrime (n-a) ]


primesR :: Int -> Int -> [Int]
primesR a b = [ x | x <- [a..b], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..limit]
  where limit = floor . sqrt . fromIntegral $ x

combinations :: [a] -> [[a]]
combinations []     = []
combinations [x]    = [[x]]
combinations (x:xs) = map (x:) (combinations xs) ++ combinations xs
