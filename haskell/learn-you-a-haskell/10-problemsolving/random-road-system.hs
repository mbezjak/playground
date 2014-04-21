import Control.Monad
import System.Random

main = do
  g <- getStdGen
  let costs = take (3 * 15) (randomRs (1, 100) g) :: [Int]
  mapM_ print costs
