
module Control.Monad.Deferral
  ( module Control.Monad.Catch
  , module Control.Monad.IO.Class
  , DeferralT(..)
  , runDeferralT, runDeferral, runDeferral'
  , defer, lift
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Alt
import Data.Functor.Bind
import Data.Functor.Plus


newtype DeferralT e m a = DeferralT { unDeferralT :: m (Either e a, m ()) }

instance Functor m => Functor (DeferralT e m) where
  fmap f = DeferralT . fmap (first (fmap f)) . unDeferralT

instance (Exception e, MonadCatch m) => Apply (DeferralT e m) where
  DeferralT f <.> DeferralT v = DeferralT $ do
    mf <- try f
    case mf of
      Left e -> return (Left e, return ())
      Right (Left e, defered) -> return (Left e, defered)
      Right (Right k, defered) -> do
        mv <- try v
        case mv of
          Left e -> return (Left e, defered)
          Right (x, defered') -> return (k <$> x, castLeft x <$> try defered' >> defered)

instance (Exception e, MonadCatch m) => Alt (DeferralT e m) where
  DeferralT f <!> DeferralT v = DeferralT $ do
    mf <- try f
    case mf of
      Right (Right x, defered) -> return (Right x, defered)
      Left e -> do
        mv <- try v
        case mv of
          Left e' -> return (Left e' >> Left e, return ())
          Right r -> return r
      Right (Left e, defered) -> do
        mv <- try v
        case mv of
          Left e' -> return (Left e' >> Left e, defered)
          Right (x, defered') -> return (x, castLeft x <$> try defered' >> defered)

instance (Exception e, MonadCatch m) => Bind (DeferralT e m) where
  DeferralT m >>- k = DeferralT $ do
    a <- try m
    case a of
      Left e -> return (Left e, return ())
      Right (Left e, defered) -> return (Left e, defered)
      Right (Right x, defered) -> do
        ka <- try . unDeferralT $ k x
        case ka of
          Left e -> return (Left e, defered)
          Right (b, defered') -> return (b, castLeft b <$> try defered' >> defered)

instance (Exception e, MonadCatch m) => Applicative (DeferralT e m) where
  pure a = DeferralT $ return (Right a, return ())
  (<*>) = (<.>)

instance (Exception e, MonadCatch m) => Monad (DeferralT e m) where
  return = pure 
  (>>=) = (>>-)

instance (Exception e, MonadCatch m, Plus m) => Plus (DeferralT e m) where
  zero = DeferralT zero  

instance (Exception e, MonadCatch m, Plus m) => Alternative (DeferralT e m) where
  empty = zero
  (<|>) = (<!>)

instance (Exception e, MonadCatch m, Plus m) => MonadPlus (DeferralT e m) where
  mzero = zero
  mplus = (<!>)

-- casts the exception type of the second argument to the exception type of the first
castLeft :: Either e a -> Either e b -> ()
castLeft _ _ = ()

-- `DeferralT` is not a fully qualified transformer as it can only be stacked on a `MonadCatch`
lift :: (Exception e, MonadCatch m) => m a -> DeferralT e m a
lift m = DeferralT $ do
  a <- try m
  return (a, return ())

instance (Exception e, MonadIO m, MonadCatch m) => MonadIO (DeferralT e m) where
  liftIO = lift . liftIO

instance (Exception e, MonadFix m, MonadCatch m) => MonadFix (DeferralT e m) where
  mfix f = DeferralT . mfix $ unDeferralT . f . either (error . show) id . fst

defer :: (Exception e, MonadCatch m) => m a -> DeferralT e m ()
defer m = DeferralT $ return (Right (), () <$ m)

runDeferral :: (Exception e, MonadCatch m) => DeferralT e m a -> m (Either e a)
runDeferral = runDeferralT id

-- for instance
--   run :: Exception e => DeferralT e IO a -> IO (Either e a)`
--   run = runDeferralT forkIO
runDeferralT :: (Exception e, MonadCatch m)
  => (m () -> m b) -> DeferralT e m a -> m (Either e a)
runDeferralT fork (DeferralT m) = do
  a <- try m
  case a of
    Left e -> return $ Left e
    Right (result, defered) -> result <$ fork (castLeft result <$> try defered)

runDeferral' :: (Exception e, MonadThrow m) => DeferralT e m a -> m a
runDeferral' (DeferralT m) = do
  (result, defered) <- m
  defered
  either throwM return result

