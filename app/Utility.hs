module Utility where

-- i.e. splitByPred (<4) [1,2,3,4,5,6,5,4,3,2,1,2,3,4,5,4,3] = [[4,5,6,5,4],[4,5,4]]
data TD = Take | Drop
splitByPred :: Eq a =>  (a -> Bool) -> [a] -> [[a]]
splitByPred pred xs = go Take [] xs where
  notp = not . pred
  go Take acc []     = []
  go Drop acc []     = [reverse acc]
  go Take acc (x:xs) = if (pred x) then go Take (x:acc) xs else go Drop [] (x:xs)
  go Drop acc (x:xs) = if (notp x) then go Drop (x:acc) xs else [reverse acc] ++ go Take [] (x:xs)

listToPairs :: [a] -> [(a,a)]
listToPairs [] = []
listToPairs (x:[]) = [] --i.e. if length of list is odd, drop last element
listToPairs (x1:x2:xs) = (x1,x2) : listToPairs xs

-- Like takeWhile, but also includes the first element where p fails
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []          =  []
takeWhileInclusive p (x:xs)
                 | p x       =  x : takeWhileInclusive p xs
                 | otherwise =  [x] -- not just []

-- takeDiagonally are used to traverse nested infinite lists
{- i.e. takeDiagonally2 4 (replicate 4 [1..4]) =
[[1,2,3,4],
 [1,2,3],
 [1,2],
 [1]] -}
-- i.e. takeDiagonally2 3 allStrands = [[[A],[C],[G]],[[A,A],[C,A]],[[A,A,A]]]
takeDiagonally2 :: Int -> [[a]] -> [[a]]
takeDiagonally2 n listoflists = zipWith (\j list -> take j list) (reverse [1..n]) listoflists

takeDiagonally :: Int -> [[[[a]]]] -> [[[[a]]]]
--breadth first search on the initial strands, then diagonalize each tree
--takeDiagonally n xs = take n $ map (map (\lsts -> takeDiagonally2 n lsts)) xs
takeDiagonally n xs = zipWith (\j lst -> map (takeDiagonally2 j) lst) (reverse [1..n]) xs

takeGenerations :: Int -> [[[[a]]]] -> [[[[a]]]]
takeGenerations n xs = map (map (take n)) xs

-- For reference, here is the source code for the two functions I used from the 'combinat' package.

-- All possible ways to choose k elements from a list, without repetitions.
choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose k [] = []
choose k (x:xs) = map (x:) (choose (k-1) xs) ++ choose k xs

-- i.e. tuples' [2,3] = [[0,0],[0,1],[0,2],[0,3],[1,0],[1,1],[1,2],[1,3],[2,0],[2,1],[2,2],[2,3]]
tuples' :: [Int] -> [[Int]]
tuples' [] = [[]]
tuples' (s:ss) = [ x:xs | x <- [0..s] , xs <- tuples' ss ]
