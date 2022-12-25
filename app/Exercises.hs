module Exercises where

import qualified Data.Set as S
import Data.Text
import Data.List
import Data.Bits
import Prelude

sumArrayElements :: [Int] -> Int
sumArrayElements [] = 0
sumArrayElements (x:xs) = x + sumArrayElements xs

-- TODO: Analyze complexity I think this is worse than O(n^2)
twoSum :: [Int] -> Int -> Bool
twoSum [] k = False
twoSum (x:[]) k = False
twoSum (x:y:[]) k = k == x + y
twoSum (x:y:xs) k 
        | k == x + y = True 
        | otherwise = twoSum (x:xs) k || twoSum (y:xs) k

-- O(n log n) Implementation 
twoSumOpt:: [Int] -> Int -> Bool
twoSumOpt [] k = False
twoSumOpt (x:[]) k = False
twoSumOpt (x:y:[]) k = k == x + y
twoSumOpt (x:y:xs) k 
        | k == x + y = True 
        | otherwise = twoSum' (y:xs) k (S.insert x seen)
    where
        seen = S.fromList([])
        twoSum' :: [Int] -> Int -> S.Set Int -> Bool
        twoSum' (x:[]) k s = S.member (k - x) s
        twoSum' (x:y:[]) k s = k == x + y
        twoSum' (x:xs) k s 
            | S.member (k - x) s = True
            | otherwise = twoSum' xs k (S.insert x s)


splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf inputList = 
    Data.List.splitAt (((Data.List.length inputList) + 1) `div` 2) inputList

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge l1 [] = l1
merge [] l2 = l2
merge (x:l1) (y:l2)
    | x < y = x:(merge l1 (y:l2))
    | otherwise = y:(merge (x:l1) l2)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort (x:[]) = [x]
mergeSort x = merge x1' x2'
    where
        (x1, x2) = splitInHalf x
        x1' = mergeSort x1
        x2' = mergeSort x2

reverseArray' :: [Int] -> [Int]
reverseArray' [] = []
reverseArray' (x:[]) = [x]
reverseArray' (x:xs) = (reverseArray' xs) ++ [x]

initArray :: Int -> [[Int]]
initArray j
    | j == 0 = []
    | otherwise = ([[]]) ++ (initArray (j -1))

updateArray :: Int -> Int -> [[Int]] -> [[Int]]
updateArray idx y [] = []
updateArray idx y arr
    | idx < 0 = arr
    | idx  > (Data.List.length arr) = arr
    | otherwise = updateArray' idx y (Data.List.length arr) arr
    where
        updateArray' :: Int -> Int -> Int -> [[Int]] -> [[Int]]
        updateArray' idx y n (x:[])
            | idx == 0 = (([(x ++ [y])]))
            | otherwise = [x]
        updateArray' idx y n arr
            | n == (idx + 1) =  (updateArray' idx y (n - 1) (Data.List.take (n - 1) arr)) ++ ([((arr !! (n - 1)) ++ [y])]) 
            | otherwise = (updateArray' idx y (n - 1) (Data.List.take (n - 1) arr)) ++ [arr !! (n - 1)]


dynamicArray' :: Int -> [String] -> Int -> [[Int]] -> [Int] -> [Int]
dynamicArray' n [] lastAnswer arr lastAnswers = lastAnswers
dynamicArray' n queries lastAnswer arr lastAnswers =
    if queryType == 1
        then dynamicArray' n (Data.List.tail queries) lastAnswer' (updateArray idx y arr) lastAnswers
    else dynamicArray' n (Data.List.tail queries) lastAnswer' arr (lastAnswers ++ [lastAnswer'])
    
    where
        query = Data.List.head queries
        parsed = parseString query
        queryType = parsed !! 0
        x = parsed !! 1
        y = parsed !! 2
        idx = mod (xor x lastAnswer) n
        lastAnswer' = if queryType == 2 
            then (arr !! idx) !! (mod y (Data.List.length (arr !! idx)))
            else lastAnswer

dynamicArray :: Int -> [String] -> [Int]
dynamicArray n queries = do
    -- Write your code here
    dynamicArray' n queries 0 (initArray n) []
    
parseString :: String -> [Int]
parseString query = Data.List.map (\x -> read x :: Int) (Data.List.words query)

-- dynamicArray' :: String -> Int -> [[Int]] -> ([Int], [[Int]])
-- dynamicArray' query lastAnswer arr =
--     | queryType == 1 = do
--         let arr' = (arr !! idx) ++ [y]
--         ([], arr') 
--     | otherwise = ([lastAnswer'], arr)
    
--     where
--         parsed = parseString query
--         queryType = parsed !! 0
--         x = parsed !! 1
--         y = parsed !! 2
--         idx = (mod (xor x lastAnswer) n)
--         lastAnswer' = (arr !! idx) !! (mod y $ Data.List.length (arr !! idx))