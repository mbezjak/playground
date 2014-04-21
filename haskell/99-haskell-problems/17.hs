split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

split' :: [a] -> Int -> ([a], [a])
split' [] _ = ([], [])
split' xs 0 = ([], xs)
split' (x:xs) n = (x:left, right)
  where (left, right) = split' xs (n-1)
