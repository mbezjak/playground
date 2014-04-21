import Data.List (group)

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map mult . group . primeFactors
  where mult a@(x:_) = (x, length a)

primeFactors :: Int -> [Int]
primeFactors = factors 2
  where factors f n
          | f*f > n        = [n]
          | n `mod` f == 0 = f:factors f (n `div` f)
          | otherwise      = factors (f+1) n
