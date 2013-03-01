import Control.Monad
import Control.Monad.Trans.Class

newtype  MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  return  = MaybeT . return . Just
  m >>= f = MaybeT $ do
    may <- runMaybeT m
    case may of
      Just x  -> runMaybeT (f x)
      Nothing -> return Nothing

instance (Monad m) => MonadPlus (MaybeT m) where
  mzero       = MaybeT $ return Nothing
  mplus ma mb = MaybeT $ do
    may <- runMaybeT ma
    case may of
      Just _  -> return may
      Nothing -> runMaybeT mb

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just
