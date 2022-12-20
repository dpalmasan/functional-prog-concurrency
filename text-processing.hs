import Data.List
import Data.Char


cleanWords :: String -> [String]
cleanWords text =
    cleanedWords
    where
        ws =  words $ map toLower text
        ws' = map (takeWhile isLetter . dropWhile (not . isLetter)) ws
        cleanedWords = filter (not . null) ws'

main =  do
    str' <- putStrLn <$> cleanedWords
    str'
    where
        text = readFile "inputs/wsj-sample.txt"
        splitText = cleanWords <$> text
        cleanedWords = unwords <$> splitText

