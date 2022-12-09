{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.Fresh (
  FreshT,
  evalFreshT, execFreshT,
  emptyFreshState, freshName, freshNames,
  Fresh,
  runFresh, evalFresh, execFresh,
) where

import Control.Monad.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Reader
import Data.Char
import Data.Tuple

import Data.Map (Map)
import qualified Data.Map as Map

{- Freshness monad transformer -}

newtype FreshT m a = FreshT { runFreshT :: StateT FreshState m a }
  deriving (Functor, Applicative, Monad, MonadTrans)

newtype FreshState = FreshState (Map String Int)

evalFreshT :: (Monad m) => FreshT m a -> FreshState -> m a
evalFreshT = evalStateT . runFreshT

execFreshT :: (Monad m) => FreshT m a -> FreshState -> m FreshState
execFreshT = execStateT . runFreshT

emptyFreshState :: FreshState
emptyFreshState = FreshState Map.empty

splitTrailingDigits :: String -> (String, String)
splitTrailingDigits s =
  let (x, y) = span isDigit (reverse s) in
  (reverse y, reverse x)

freshName :: (Monad m) => String -> FreshT m (String, Int)
freshName n = FreshT $ StateT $ \(FreshState st) ->
  let count = Map.findWithDefault 0 n st in
  return ((n, count), FreshState $ Map.insert n (count+1) st)

freshNames :: (Monad m) => String -> Int -> FreshT m [(String, Int)]
freshNames n k = FreshT $ StateT $ \(FreshState st) ->
  let count = Map.findWithDefault 0 n st in
  return (map (\x -> (n, x)) [0..k-1], FreshState $ Map.insert n (count+1) st)

{- Instances -}

--instance (MonadReader r m) => MonadReader r (FreshT m) where
--  ask = lift ask
--  local f (FreshT x) = FreshT (local f x)
-- error: Illegal instance, type not of the form (T a1 ... an) (it is though??)

{- Freshness monad -}

type Fresh = FreshT Identity

runFresh :: Fresh a -> FreshState -> (a, FreshState)
runFresh = runState . runFreshT

evalFresh :: Fresh a -> FreshState -> a
evalFresh = evalState . runFreshT

execFresh :: Fresh a -> FreshState -> FreshState
execFresh = execState . runFreshT
