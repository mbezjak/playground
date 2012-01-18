import Control.Monad

listOfTuples :: [(Int,Char)]
listOfTuples = [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

listOfTuples' :: [(Int,Char)]
listOfTuples' = do
  n <- [1,2]
  ch <- ['a','b']
  return (n,ch)

listOfTuples'' :: [(Int,Char)]
listOfTuples'' = [ (n,ch) | n <- [1,2], ch <- ['a','b'] ]

guard' :: (MonadPlus m) => Bool -> m ()
guard' True  = return ()
guard' False = mzero

sevensOnly :: [Int]
sevensOnly = [1..50] >>= \x -> guard' ('7' `elem` show x) >> return x

sevensOnly' :: [Int]
sevensOnly' = do
  x <- [1..50]
  guard' ('7' `elem` show x)
  return x

sevensOnly'' :: [Int]
sevensOnly'' = [ x | x <- [1..50], '7' `elem` show x ]
