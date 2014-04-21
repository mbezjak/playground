import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
  in (firstCoin, secondCoin, thirdCoin)

threeCoins' :: StdGen -> (Bool, Bool, Bool)
threeCoins' gen = asTuple3 . take 3 $ randoms gen
  where asTuple3 [a, b, c] = (a, b, c)

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (a, newGen) = random gen in a:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Eq n, Num n) => n -> g -> ([a], g)
finiteRandoms 0 g = ([], g)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n-1) newGen
  in (value:restOfList, finalGen)
