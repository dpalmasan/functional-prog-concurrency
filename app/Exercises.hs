import qualified Data.Set as S
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
