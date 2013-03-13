import System.Random
import Control.Monad.State

removeAt :: Int -> [a] -> [a]
removeAt _ []     = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x:removeAt (n-1) xs


diff_select :: Int -> Int -> IO [Int]
diff_select n m = getStdRandom (select n [1..m])

select :: RandomGen g => Int -> [Int] -> g -> ([Int], g)
select _ [] g             = ([], g)
select 0 _  g             = ([], g)
select remaining pool gen = (pool!!idx:rest, newGen')
  where (idx, newGen)   = randomR (0, length pool - 1) gen
        (rest, newGen') = select (remaining-1) (removeAt idx pool) newGen


diff_select' :: Int -> Int -> IO [Int]
diff_select' n m = getStdRandom (select' n [1..m])

select' :: RandomGen g => Int -> [Int] -> g -> ([Int], g)
select' _ [] g           = ([], g)
select' 0 _  g           = ([], g)
select' remaining pool g = flip runState g $ do
  idx  <- state $ randomR (0, length pool - 1)
  rest <- state $ select' (remaining-1) (removeAt idx pool)
  return $ pool!!idx:rest
