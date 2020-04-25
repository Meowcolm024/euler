-- Storing useful functions
module Func where

getDiv :: Int -> [Int]
getDiv n
    | sq == n   = divs ++ reverse (map (n`div`) (init divs))
    | otherwise = divs ++ reverse (map (n`div`) divs)
    where divs  = [x | x <- [1..floor(sqrt(fromIntegral n))], n `mod` x == 0]
          sq    = floor(sqrt(fromIntegral n)) ^ 2

isPrime :: Integer -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

fromStr :: String -> [Int]
fromStr n = [read [x] :: Int | x <- n, x /= ' ']

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

ele :: Int -> [Int] -> Bool
ele _ [] = False
ele n (x:xs) | n == x = True
             | n < x  = False
             | otherwise = ele n xs

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
          