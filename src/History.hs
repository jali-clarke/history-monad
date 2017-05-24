{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module History (
    History,
    runHistory,

    module Control.Monad.State.Lazy
) where

import Control.Monad.State.Lazy

data History s a = History (s -> a) ([s -> s] -> [s -> s])

runHistory :: History s a -> s -> a
runHistory (History proj stack) = proj . foldr (.) id (stack [])

instance Functor (History s) where
    fmap f (History proj stack) = History (f . proj) stack

instance Applicative (History s) where
    pure a = History (const a) id
    History projf stackf <*> History proja stacka = History (projf <*> proja) (stacka . stackf)

instance Monad (History s) where
    History proja stacka >>= g = History projb stackb
        where
            projh = g . proja
            projb s = let History projb' _ = projh s in projb' s
            stackb = undefined

instance MonadState s (History s) where
    get = History id id
    put s = History (const ()) (const s :)
    state func = History (fst . func) (snd . func :)
