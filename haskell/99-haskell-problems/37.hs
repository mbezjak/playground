import Data.List (group)

phi :: Int -> Int
phi 1 = 1
phi x = product . map calc . primeFactorsMult $ x
  where calc (p, m) = (p - 1) * p ^ (m-1)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map mult . group . primeFactors
  where mult a@(x:_) = (x, length a)

primeFactors :: Int -> [Int]
primeFactors = factors 2
  where factors f n
          | f*f > n        = [n]
          | n `mod` f == 0 = f:factors f (n `div` f)
          | otherwise      = factors (f+1) n
