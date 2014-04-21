import Data.List (group)

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\a -> (length a, head a)). group
