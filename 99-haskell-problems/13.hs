data Encoding a = Single a | Multiple Int a
                deriving Show

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect []     = []
encodeDirect (x:xs) = (encode (1 + length same) x):encodeDirect rest
  where (same, rest) = span (==x) xs
        encode 1 a = Single a
        encode n a = Multiple n a
