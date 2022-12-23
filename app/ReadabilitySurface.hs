module ReadabilitySurface (cleanWords, countUnique) where

import qualified Data.Text as T
import Data.List
import Data.Char
import Prelude

cleanWords :: T.Text -> [T.Text]
cleanWords text =
    cleanedWords
    where
        ws =  T.words text
        notLetter = not . isLetter
        keepLetters = T.toLower . T.dropAround notLetter
        ws' = map keepLetters ws
        cleanedWords = filter (not . T.null) ws'

countUnique :: T.Text -> Int
countUnique text =
    length . map head . group . sort $ wordList
    where
        wordList = cleanWords text

countWords :: T.Text -> Int
countWords text =
    length $ wordList
    where
        wordList = cleanWords text

typeTokenRatio :: T.Text -> Double
typeTokenRatio text =
    result
    where
        unique = fromIntegral $ countUnique text
        total = fromIntegral $ countWords text
        result = unique/total