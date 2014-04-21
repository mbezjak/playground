data Encoding a = Single a | Multiple Int a
                deriving Show

decodeModified :: [Encoding a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs)     = x:decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

decodeModified' :: [Encoding a] -> [a]
decodeModified' = foldr decode []
  where decode (Single x)     = (x:)
        decode (Multiple n x) = (replicate n x ++)
