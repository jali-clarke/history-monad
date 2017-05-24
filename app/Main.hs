module Main where

import History

histInit :: History String ()
histInit = put "World!" *> bookmark *> put "Hello"

reverseHistory :: History String ()
reverseHistory =
    do
        str0 <- get
        rewind 1
        str1 <- get
        let rts1 = reverse str1
            rts0 = reverse str0
        put rts0
        bookmark
        put rts1

readHistory :: History String String
readHistory = (++) <$> get <*> remembering 1 get

main :: IO ()
main =
    do
        let (h0, _) = runHistory (histInit *> readHistory) ""
        putStrLn h0
        let (h1, _) = runHistory (histInit *> reverseHistory *> readHistory) ""
        putStrLn h1
