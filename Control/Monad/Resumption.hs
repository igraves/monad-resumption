module Control.Monad.Resumption where

import Control.Monad
import Control.Monad.Trans


newtype ResT m a = ResT (m (Either a (ResT m a)))

runResT :: (Monad m) => ResT m a -> m a
runResT (ResT m)  = do
                      x <- m
                      case x of 
                        Left val -> return val
                        Right m -> runResT m

instance Monad m => Monad (ResT m) where
  return x = ResT $ return $ Left x
  ResT m >>= f =  ResT $ do 
                          x <- m 
                          case x of
                             Left  val -> return $ Right $ f val 
                             Right res -> return $ Right $ res >>= f 

instance MonadTrans ResT where
  lift m = ResT (m >>= return . Left)
