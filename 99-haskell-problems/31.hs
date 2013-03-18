isPrime :: Int -> Bool
isPrime n = null . filter (\x -> n `mod` x == 0) $ [2..limit]
  where limit = (abs n) - 1

isPrime' :: Int -> Bool
isPrime' n = null . filter (\x -> n `mod` x == 0) $ [2..limit]
  where limit = floor . sqrt . fromIntegral $ n
