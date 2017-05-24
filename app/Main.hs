module Main where

import History

-- numOps should be 2, flip runHistory 0 should return 3
test :: History Int Int
test = do
    i <- get
    put (i + 1)
    i' <- get
    put (i + 2)
    get

main :: IO ()
main = do
    print (numOps test)
    print (runHistory test 0)
