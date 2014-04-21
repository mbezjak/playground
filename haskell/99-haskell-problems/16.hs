dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) first ++ dropEvery rest n
  where (first, rest) = splitAt n xs

dropEvery' :: [a] -> Int -> [a]
dropEvery' xs n = map fst . filter notN . zip xs $ pos
  where pos = cycle [1..n]
        notN (_,i) = i /= n
