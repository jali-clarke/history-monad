{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HistoryT (
    HistoryT(),
    runHistoryT,

    History,
    runHistory
) where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State.Lazy

newtype HistoryT s m a = HistoryT {runHistoryT' :: StateT [s] m a} deriving (Functor, Applicative, Monad, MonadTrans)

runHistoryT :: HistoryT s m a -> (s -> m (a, [s]))
runHistoryT (HistoryT comp) = runStateT comp . (: [])

type History s = HistoryT s Identity

runHistory :: History s a -> (s -> (a, [s]))
runHistory comp = runIdentity . runHistoryT comp
