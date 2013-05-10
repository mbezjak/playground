import qualified Data.Map as Map

phoneBook =
  [("betty","555-2938")
  ,("bonnie","452-2928")
  ,("patsy","493-2928")
  ,("lucille","205-2928")
  ,("wendy","939-8282")
  ,("penny","853-2492")
  ]

findByKey :: Eq k => k -> [(k,v)] -> Maybe v
findByKey _ [] = Nothing
findByKey key xs
  | null matching = Nothing
  | otherwise     = Just (snd . head $ matching)
  where matching  = filter (\(k,v) -> key == k) xs

findByKey' :: Eq k => k -> [(k,v)] -> Maybe v
findByKey' _ [] = Nothing
findByKey' key ((k,v):xs)
  | key == k = Just v
  | otherwise = findByKey key xs

findByKey2 :: Eq k => k -> [(k,v)] -> Maybe v
findByKey2 key = foldr (\(k,v) acc -> if k == key then Just v else acc) Nothing

fromList' :: Ord k => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty

phoneBook2 =
    [("betty","555-2938")
    ,("betty","342-2492")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("patsy","943-2929")
    ,("patsy","827-9162")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ,("penny","555-2111")
    ]

phoneBookToMap :: Ord k => [(k,String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\f s -> f ++ "," ++ s)

phoneBookToMap2 :: Ord k => [(k,a)] -> Map.Map k [a]
phoneBookToMap2 = Map.fromListWith (++) . map (\(k,v) -> (k,[v]))
