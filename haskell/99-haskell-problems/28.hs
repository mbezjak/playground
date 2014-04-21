import Data.List (sortBy, groupBy)
import Data.Function (on)

lsort :: [[a]] -> [[a]]
lsort = sortBy (compare `on` length)

lfsort :: [[a]] -> [[a]]
lfsort = concat . lsort . groupBy ((==) `on` length) . lsort
