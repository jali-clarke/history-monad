{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module History (

) where

import Control.Monad.State.Lazy (MonadState(..))

data History s a = History {project :: s -> a, evoStack :: [s -> s] -> [s -> s]}

instance Functor (History s) where
    fmap f (History proj stack) = History (f . proj) stack

instance Applicative (History s) where
    pure a = History (const a) id
    History projf stackf <*> History proja stacka = History (projf <*> proja) (stacka . stackf)

instance MonadState s (History s) where
    get = History id id
    put s = History (const ()) (const s :)
    state func = History (fst . func) (snd . func :)
