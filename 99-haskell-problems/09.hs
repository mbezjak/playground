import Data.List (group)

pack :: Eq a => [a] -> [[a]]
pack = group

pack' :: Eq a => [a] -> [[a]]
pack' []     = []
pack' (x:xs) =
  let (same, rest) = span (==x) xs in
  (x:same):pack' rest
