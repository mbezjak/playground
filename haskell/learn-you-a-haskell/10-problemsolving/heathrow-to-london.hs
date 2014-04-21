data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)
type Path  = [(Label, Int)]

pathCost :: Path -> Int
pathCost path = sum $ map snd path

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let priceA          = pathCost pathA
      priceB          = pathCost pathB
      forwardPriceToA = priceA + a
      crossPriceToA   = priceB + b + c
      forwardPriceToB = priceB + b
      crossPriceToB   = priceA + a + c
      newPathToA      = if forwardPriceToA <= crossPriceToA
                        then (A,a):pathB
                        else (C,c):(B,b):pathB
      newPathToB      = if forwardPriceToB <= crossPriceToB
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
  in (newPathToA, newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roads = let (pathA, pathB) = foldl roadStep ([], []) roads
                    in if pathCost pathA <= pathCost pathB
                       then reverse pathA
                       else reverse pathB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

roadSystemFromText :: String -> RoadSystem
roadSystemFromText text = map (\[a,b,c] -> Section a b c) threes
  where threes = groupsOf 3 . map read . lines $ text

main = do
  contents <- getContents
  let optimal    = optimalPath . roadSystemFromText $ contents
      pathString = concat $ map (show . fst) optimal
      cost       = pathCost optimal
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "The price is: " ++ show cost
