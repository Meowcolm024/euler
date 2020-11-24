-- Storing useful functions
module Func where

import Data.Char (digitToInt)
import Control.Monad (join, ap)

fromStr :: String -> [Int]
fromStr n = [ read [x] :: Int | x <- n, x /= ' ' ]

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

ele :: Int -> [Int] -> Bool
ele _ [] = False
ele n (x : xs) | n == x    = True
               | n < x     = False
               | otherwise = ele n xs

listToInt :: Num p => [p] -> p
listToInt [] = 0
listToInt (x:xs) = x * 10 ^ (length xs) + listToInt xs

intToList :: Int -> [Int]
intToList = map (digitToInt) . show

primes :: [Integer]
primes = 2 : filter isPrime [3 ..]

isPrime :: Integer -> Bool
isPrime n = foldr (\x acc -> (x*x > n) || (n `rem` x /= 0 && acc)) False primes

isqrt :: Integral a => a -> Bool
isqrt = ap (==) (join (*) . floor . sqrt . fromIntegral)
