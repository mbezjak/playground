import System.Random
import Control.Monad (replicateM)

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  indices <- replicateM n $ getStdRandom (randomR limits)
  return $ map (xs!!) indices
  where limits = (0, length xs - 1)
