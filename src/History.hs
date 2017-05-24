{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module History (
    History,
    runHistory,
    numOps,

    module Control.Monad.State.Lazy
) where

import Control.Monad.State.Lazy

data History s a = Pure a | Modify (s -> s) (History s a) | Get (s -> History s a)

runHistory :: History s a -> s -> a
runHistory (Pure a) _ = a
runHistory (Modify f newHist) s = runHistory newHist (f s)
runHistory (Get f) s = runHistory (f s)
