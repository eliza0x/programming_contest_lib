module Residue where

fact :: Int -> Int
fact 0 = 1
fact n = n |* fact (n|-1)

modN :: Int
modN = 10^9+7
 
(|+) :: Int -> Int -> Int
(|+) n m = (`mod` modN) $ n + m 

(|-) :: Int -> Int -> Int
(|-) n m = (`mod` modN) $ n - m 

(|*) :: Int -> Int -> Int
(|*) n m = (`mod` modN) $ (n `mod` modN) * (m `mod` modN) 

(|^) :: Int -> Int -> Int
(|^) x n
  | n == 0    = 1
  | even n    = mod (x*x) modN |^ div n 2
  | otherwise = (`mod` modN) $ (x |^ (n - 1)) * x

(|/) :: Int -> Int -> Int
(|/) n m = n |* (m |^ (modN - 2))

