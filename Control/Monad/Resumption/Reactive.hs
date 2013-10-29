module Control.Monad.Resumption.Reactive where

import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Monad.IO.Class


newtype ReacT input output m a = 
        ReacT { deReacT :: m (Either a (output, input -> ReacT input output m a)) }

instance Monad m => Monad (ReacT input output m) where
  return = ReacT . return . Left
  ReacT comp >>= f = ReacT $ do
                                inner <- comp
                                case inner of
                                    Left v                 -> deReacT (f v)
                                    Right (output, resume) -> return (Right (output,\ p -> resume p >>= f))

instance MonadTrans (ReacT input output) where
  lift m = ReacT $ m >>= return . Left

instance Monad m => Functor (ReacT input output m) where
  fmap f (ReacT m) = undefined

instance Monad m => Applicative (ReacT input output m) where
  pure  = return
  (<*>) = ap

instance MonadIO m => MonadIO (ReacT input output m) where
  liftIO = lift . liftIO
