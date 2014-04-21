compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x:(compress $ dropWhile (==x) xs)
