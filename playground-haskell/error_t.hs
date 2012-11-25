import Control.Monad.Error

eio :: IO (Either String Int)
eio = return (Right 1)

eiof :: IO (Either String Int)
eiof = return (Left "Parse failure")

mio :: IO (Maybe Int)
mio = return (Just 2)

miof :: IO (Maybe Int)
miof = return Nothing

et :: IO (Either String Int) -> ErrorT String IO Int
et = ErrorT

mt :: IO (Maybe Int) -> ErrorT String IO Int
mt = ErrorT . fmap (maybeToEither "File not found")

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither = flip maybe Right . Left

run :: IO (Either String Int) -> IO (Maybe Int) -> ErrorT String IO Int
run e m = do
  x <- et e
  y <- mt m
  return (x + y)

pt :: Int -> ErrorT String IO Int
pt 0 = throwError "Pick another"
pt n = return n

runp :: Int -> Int -> ErrorT String IO Int
runp a b = do
  x <- pt a
  y <- pt b
  return (x + y)


test1 :: IO (Either String Int)
test1 = runErrorT (run eio mio)

test2 :: IO (Either String Int)
test2 = runErrorT (run eiof mio)

test3 :: IO (Either String Int)
test3 = runErrorT (run eio miof)

test4 :: IO (Either String Int)
test4 = runErrorT (runp 1 2)

test5 :: IO (Either String Int)
test5 = runErrorT (runp 0 2)
