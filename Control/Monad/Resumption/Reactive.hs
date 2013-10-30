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
  fmap f (ReacT m) = ReacT (m >>= \ r -> case r of
                                           Left v      -> return (Left (f v))
                                           Right (o,k) -> return (Right (o,\ i -> fmap f (k i))))

instance Monad m => Applicative (ReacT input output m) where
  pure  = return
  (<*>) = ap

instance MonadIO m => MonadIO (ReacT input output m) where
  liftIO = lift . liftIO

stepRe :: Monad m => output -> (input -> m a) -> ReacT input output m a
stepRe o k = ReacT (return (Right (o,\ i -> lift (k i))))

signal :: Monad m => output -> ReacT input output m input
signal o = ReacT (return (Right (o,return)))