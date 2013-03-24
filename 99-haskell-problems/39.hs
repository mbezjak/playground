primesR :: Int -> Int -> [Int]
primesR a b = [ x | x <- [a..b], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..limit]
  where limit = floor . sqrt . fromIntegral $ x
