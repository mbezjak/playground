import Data.List ((\\))

group3 :: Eq a => [a] -> [[[a]]]
group3 xs = do
  ys <- combinations 2 xs
  zs <- combinations 3 (xs \\ ys)
  ws <- combinations 4 ((xs \\ ys) \\ zs)
  return [ys, zs, ws]

group :: Eq a => [Int] -> [a] -> [[[a]]]
group [] _      = [[]]
group (coeff:cs) xs = do
  comb <- combinations coeff xs
  rest <- group cs (xs \\ comb)
  return (comb:rest)


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) =
  map (x:) (combinations (n-1) xs) ++ combinations n xs
