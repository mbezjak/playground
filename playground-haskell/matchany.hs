data Action = Match String | ConsumeAny deriving (Show)

match :: String -> String -> Bool
match str pattern = matchAction str (actions pattern)

matchAction :: String -> [Action] -> Bool
matchAction str [] = null str
matchAction "" [ConsumeAny] = True
matchAction "" (_:_) = False
matchAction str (Match s : as) =
  case consume str s of
    Nothing        -> False
    Just (_, rest) -> matchAction rest as
matchAction str all@(ConsumeAny:as) =
  (matchAction str as) || (matchAction (tail str) all)

consume :: String -> String -> Maybe (String, String)
consume str exact =
  if (fst ms) == exact then Just ms else Nothing
  where ms = splitAt (length exact) str

actions :: String -> [Action]
actions ""       = []
actions ('*':xs) = ConsumeAny : actions xs
actions xs       = Match (fst ms) : actions (snd ms)
  where ms = span (/='*') xs


tests = and [ match "foo" "foo"
            , not $ match "foox" "foo"
            , not $ match "foo" "foox"
            , match "foo" "foo*"
            , match "foox" "foo*x"
            , match "foo1x" "foo*x"
            , match "foo123x" "foo*x"
            , match "foo123x45x" "foo*x"
            , match "foo123x45x6y" "foo*x*y"
            , not $ match "foo123x45x6y7x" "foo*x*y"
            , match "foo123x45x6y7x8y" "foo*x*y"
            , match "foo123x456x7y" "foo*x*"
            ]
