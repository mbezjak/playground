import Data.List

mean :: (Real a, Fractional b) => [a] -> b
mean xs = realToFrac (sum xs) / genericLength xs
