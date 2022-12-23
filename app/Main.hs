module Main where

import qualified ReadabilitySurface as RS
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
    print =<< uniqueWords
    where
        text = TIO.readFile "inputs/wsj-sample.txt"
        uniqueWords = RS.countUnique <$> text
