import Data.List (group)

pack :: Eq a => [a] -> [[a]]
pack = group

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\a -> (length a, head a)). group

data Encoding a = Single a | Multiple Int a
                deriving Show

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map toEncoding . encode
  where toEncoding (1, x) = Single x
        toEncoding (n, x) = Multiple n x
