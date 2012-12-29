import Data.List

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs

quadraticDist :: (Real a, Fractional a) => [a] -> a
quadraticDist xs = sum . map dist $ xs
  where m = mean xs
        dist x = (x - m) ^ 2

class VarianceGen v where
  var :: (Real a, Fractional a) => v a -> a

newtype Population a = Population [a] deriving Show
newtype Sample a     = Sample [a]     deriving Show

instance VarianceGen Population where
  var (Population xs) = quadraticDist xs / n
    where n = genericLength xs

instance VarianceGen Sample where
  var (Sample xs) = quadraticDist xs / (n-1)
    where n = genericLength xs

stddev :: (Real a, Floating a, VarianceGen v) => v a -> a
stddev xs = sqrt (var xs)

popstat :: (Real a, Floating a) => [a] -> (a, a, a)
popstat xs = (mean xs, var pop, stddev pop)
  where pop = Population xs

samplestat :: (Real a, Floating a) => [a] -> (a, a, a)
samplestat xs = (mean xs, var sample, stddev sample)
  where sample = Sample xs
