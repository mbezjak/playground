primeFactors :: Int -> [Int]
primeFactors = factors 2
  where factors f n
          | f >= n       = [n]
          | gcd f n == f = f:factors f (n `div` f)
          | otherwise    = factors (f+1) n

primeFactors' :: Int -> [Int]
primeFactors' = factors 2
  where factors f n
          | f*f > n        = [n]
          | n `mod` f == 0 = f:factors f (n `div` f)
          | otherwise      = factors (f+1) n
