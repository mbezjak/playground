import qualified Data.Map as Map

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' [] where
  fmap' = map

instance Functor' Maybe where
  fmap' f Nothing  = Nothing
  fmap' f (Just x) = Just (f x)

instance Functor' (Either a) where
  fmap' f (Left x) = Left x
  fmap' f (Right x) = Right (f x)

instance Functor' (Map.Map k) where
  fmap' = Map.map
