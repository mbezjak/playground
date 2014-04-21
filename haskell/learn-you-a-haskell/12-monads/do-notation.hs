import Control.Monad

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

foo'' :: Maybe String
foo'' = do
  a <- Nothing
  Just "hello"

marySue :: Maybe Bool
marySue = Just 9 >>= (\x -> Just (x > 8))

marySue' :: Maybe Bool
marySue' = do
  x <- Just 9
  Just (x > 8)

justH :: Maybe Char
justH = do
  (x:xs) <- Just "hello"
  return x

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x
