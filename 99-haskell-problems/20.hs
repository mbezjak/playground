removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (z, before ++ zs)
  where (before, z:zs) = splitAt (n-1) xs

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ []     = (Nothing, [])
removeAt' 1 (x:xs) = (Just x, xs)
removeAt' n (x:xs) = (el, x:rest)
  where (el, rest) = removeAt' (n-1) xs
