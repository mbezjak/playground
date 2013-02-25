myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = reverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' = foldr (\a-> (++[a])) []
