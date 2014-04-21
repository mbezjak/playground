insertAt :: a -> [a] -> Int -> [a]
insertAt a [] _          = [a]
insertAt a xs n | n <= 1 = a:xs
insertAt a (x:xs) n      = x:insertAt a xs (n-1)
