import Data.List (stripPrefix,groupBy)
import Control.Applicative ((<|>))

data Action = Match String
            | MatchAny deriving (Show)

match :: String -> String -> Bool
match string pattern =
  consume (Just string) (actions . distinguish $ pattern) == Just ""

consume :: Maybe String -> [Action] -> Maybe String
consume Nothing _              = Nothing
consume m []                   = m >>= \s -> if null s then m else Nothing
consume m [MatchAny]           = m >> return ""
consume m (Match p : as)       = consume (m >>= stripPrefix p) as
consume m as@(MatchAny : rest) = consume m rest <|> consume (m >>= maybeTail) as

actions :: [String] -> [Action]
actions = foldr action []
  where action "*" = (MatchAny:)
        action x   = (Match x:)

distinguish :: String -> [String]
distinguish = groupBy (\a b -> a /= '*' && b /= '*')

maybeTail :: [a] -> Maybe [a]
maybeTail []     = Nothing
maybeTail (_:xs) = Just xs

tests :: Bool
tests = and . map snd $ testCases

testCases :: [(Int,Bool)]
testCases =
  zip [0..] [ match "foo" "foo"
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
