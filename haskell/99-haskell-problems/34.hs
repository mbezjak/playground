totient :: Int -> Int
totient 1 = 1
totient m = length . filter (coprime m) $ [1..m-1]

coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1
