module Main where

import History

{-
-- runHistory test 0 should return 3
test :: History Int Int
test = do
    i <- get
    put (i + 1)
    i' <- get
    put (i + 2)
    get
-}

main :: IO ()
main = {-print (runHistory test 0)-} return ()
