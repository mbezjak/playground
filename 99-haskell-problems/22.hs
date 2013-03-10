range :: Int -> Int -> [Int]
range n m = [n..m]

range' :: Int -> Int -> [Int]
range' n m | n == m    = [n]
           | n < m     = n:range' (n+1) m
           | otherwise = n:range' (n-1) m

range'' :: Int -> Int -> [Int]
range'' n m
  | n == m    = [n]
  | otherwise = n:range'' (n+offset) m
  where offset = negate . signum $ n-m
