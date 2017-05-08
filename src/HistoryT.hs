{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

module HistoryT (
    HistoryT(),
    runHistoryT,

    History,
    runHistory,

    module Control.Monad.State.Class,

    bookmark,
    remembering,
    rewind
) where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State.Class

newtype HistoryT s m a = HistoryT {runHistoryT' :: StateT [s] m a} deriving (Functor, Applicative, Monad, MonadTrans)

runHistoryT :: HistoryT s m a -> (s -> m (a, [s]))
runHistoryT (HistoryT comp) = runStateT comp . (: [])

type History s = HistoryT s Identity

runHistory :: History s a -> (s -> (a, [s]))
runHistory comp = runIdentity . runHistoryT comp

instance Monad m => MonadState s (HistoryT s m) where
    get = HistoryT (gets head) -- heh
    put s = HistoryT (modify $ (s :) . tail)

bookmark :: Monad m => HistoryT s m s
bookmark = HistoryT . state $ \ss@(s : _) -> (s, s : ss)

remembering :: Monad m => Int -> HistoryT s m a -> HistoryT s m a
remembering n (HistoryT comp) = HistoryT $
    do
        ss <- get
        let len = length ss
        put $ drop (min n (len - 1)) ss
        a <- comp
        modify . (++) $ take (min n (len - 1)) ss
        return a

rewind :: Monad m => Int -> HistoryT s m s
rewind n = HistoryT . state $ \ss -> let ss' = drop (min n (length ss - 1)) ss in (head ss', ss')
