rotate :: [a] -> Int -> [a]
rotate [] _         = []
rotate xs 0         = xs
rotate xs n | n < 0 = rotate xs (n `mod` length xs)
rotate (x:xs) n     = rotate (xs ++ [x]) (n-1)
