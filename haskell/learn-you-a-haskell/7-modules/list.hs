import Data.List

numUniques :: Eq a => [a] -> Int
numUniques = length . nub

search :: Eq a => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

removeConsecutive :: Eq a => [a] -> [a]
removeConsecutive []  = []
removeConsecutive [x] = [x]
removeConsecutive (x:y:rest)
  | x == y    = removeConsecutive (x:rest)
  | otherwise = x : removeConsecutive (y:rest)

on' :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on' f g = \x y -> f (g x) (g y)
