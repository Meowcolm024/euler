-- Storing useful functions
module Func where

fromStr :: String -> [Int]
fromStr n = [ read [x] :: Int | x <- n, x /= ' ' ]

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

ele :: Int -> [Int] -> Bool
ele _ [] = False
ele n (x : xs) | n == x    = True
               | n < x     = False
               | otherwise = ele n xs

primes :: [Integer]
primes = filter isPrime [2 ..]
  where
    isPrime n = go 2
      where
        go d | d * d > n      = True
             | n `rem` d == 0 = False
             | otherwise      = go (d + 1)

listToInt :: Num p => [p] -> p
listToInt [] = 0
listToInt (x:xs) = x * 10 ^ (length xs) + listToInt xs
