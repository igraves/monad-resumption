module Control.Monad.Resumption.Connectors where

import Control.Monad.Resumption.Reactive

-- | The parallel operator for combining computations in ReacT that share 
--   the same underlying monad and halting types.  No guarantees are given to 
--   which device's halting message will be seen by a handler.
(<||>) :: (Monad m) => ReacT i1 o1 m a -> ReacT i2 o2 m a -> ReacT (i1,i2) (o1,o2) m a
(<||>) (ReacT l) (ReacT r) = ReacT $ do
                                      l' <- l
                                      r' <- r
                                      case (l',r') of
                                          (Left a,_)                        -> return $ Left a
                                          (_,Left a)                        -> return $ Left a
                                          (Right (o1,res1),Right (o2,res2)) -> return $ Right ((o1,o2),\(i1,i2) -> (res1 i1) <||> (res2 i2))

-- | The refold operator changes the output and input types of a reactive resumption
refold :: (Monad m) => (o1 -> o2) -> (o1 -> i2 -> i1) -> ReacT i1 o1 m a -> ReacT i2 o2 m a
refold otpt inpt (ReacT r) = ReacT $ do
                                        r' <- r
                                        case r' of
                                          Left a          -> return $ Left a
                                          Right (o1,res1) -> return $ Right (otpt o1, \i2 -> refold otpt inpt (res1 (inpt o1 i2)))

-- | Chains two reactive resumptions together in a pipelined fashioned.  That is, inputs
--   and outputs are passed along between devices "tickwise".
pipeline :: (Monad m) => ReacT i z m a -> ReacT z o m a -> ReacT i o m a
pipeline r1 r2 = let r' = r1 <||> r2
                  in refold snd pipe r'
  where
    pipe oldout newinp = (newinp,(fst oldout))
