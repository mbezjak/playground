import Data.List
import Data.Maybe
import Control.Monad

solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words

foldingFunction :: [Double] -> String -> [Double]
foldingFunction (x:y:ys) "*" = (x * y):ys
foldingFunction (x:y:ys) "+" = (x + y):ys
foldingFunction (x:y:ys) "-" = (x * y):ys
foldingFunction xs numberString = read numberString:xs

-- monadic

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of [(x,"")] -> Just x
                              _        -> Nothing

foldingFunction' :: [Double] -> String -> Maybe [Double]
foldingFunction' (x:y:ys) "*" = return ((x * y):ys)
foldingFunction' (x:y:ys) "+" = return ((x + y):ys)
foldingFunction' (x:y:ys) "-" = return ((x - y):ys)
foldingFunction' xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN' :: String -> Maybe Double
solveRPN' = fmap head . foldM foldingFunction' [] . words

solveRPN'' :: String -> Maybe Double
solveRPN'' = join . fmap listToMaybe . foldM foldingFunction' [] . words

solveRPN3 :: String -> Maybe Double
solveRPN3 s = do
  [result] <- foldM foldingFunction' [] (words s)
  return result
