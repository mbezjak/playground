import Data.Ratio
import Data.List (all, partition)

values = [(3,1%2),(5,1%4),(9,1%4)]

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
    [(Prob [('a',1%2),('b',1%2)], 1%4)
    ,(Prob [('c',1%2),('d',1%2)], 3%4)
    ]

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob ys,p) = map (\(x,r) -> (x,p*r)) ys

instance Monad Prob where
  return x = Prob [(x,1%1)]
  m >>= f  = flatten (fmap f m)
  fail _   = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (==Tails) [a,b,c])

testThree :: [(Bool,Rational)]
testThree = foldr addSame [(True,0),(False,0)] (getProb flipThree)
  where addSame (f,p) [(_,p1),(_,p2)]
          | f         = [(True, p+p1),(False,p2)]
          | otherwise = [(True, p1),(False,p+p2)]

testThree' :: [(Bool,Rational)]
testThree' = let result = getProb flipThree in [sumWhen True result, sumWhen False result]
  where sumWhen flag xs = (flag, sum . map snd . onlyWith flag $ xs)
        onlyWith flag = filter (\(c,_) -> c == flag)
