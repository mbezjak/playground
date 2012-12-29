newtype L a = L { runL :: [a] } deriving Show

instance Monad L where
  return x = L [x]
  m >>= f  = L $ do
    x <- runL m
    runL . f $ x

newtype M a = M { runM :: Maybe a } deriving Show

instance Monad M where
  return  = M . Just
  m >>= f = M (runM m >>= runM . f)
