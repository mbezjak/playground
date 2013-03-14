import System.Random
import Data.List (permutations)

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  idx <- randomRIO (0, length ps - 1)
  return $ ps !! idx
  where ps = permutations xs

rnd_permu' :: [a] -> IO [a]
rnd_permu' [] = return []
rnd_permu' xs = do
  idx  <- randomRIO (0, length xs - 1)
  rest <- rnd_permu' (removeAt idx xs)
  return $ (xs!!idx):rest


removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x:removeAt (n-1) xs


-- inefficient but working implementation for permutations
permu :: [a] -> [[a]]
permu [] = [[]]
permu xs = pairs xs >>= \(y,ys) -> map (y:) (permu ys)

pairs :: [a] -> [(a, [a])]
pairs [x] = [(x,[])]
pairs (x:xs) = (x,xs):(map (\(y,ys) -> (y, x:ys)) (pairs xs))
