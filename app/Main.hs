module Main where

import qualified ReadabilitySurface as RS

main :: IO ()
main = do
    print =<< uniqueWords
    where
        text = readFile "inputs/wsj-sample.txt"
        cleanedWords = RS.cleanWords <$> text
        uniqueWords = RS.countUnique <$> cleanedWords
