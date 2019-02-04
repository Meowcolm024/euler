-- Storing useful functions
module Func where

getDiv :: Int -> [Int]
getDiv n
    | sq == n   = divs ++ reverse (map (n`div`) (init divs))
    | otherwise = divs ++ reverse (map (n`div`) divs)
    where divs  = [x | x <- [1..floor(sqrt(fromIntegral n))], n `mod` x == 0]
          sq    = floor(sqrt(fromIntegral n)) ^ 2

isPrime :: Int -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

fromStr :: String -> [Int]
fromStr n = [read [x] :: Int | x <- n, x /= ' ']

dp_fib :: [Integer] -> Int -> Int -> Integer
dp_fib fibs n m
    | n < m     = dp_fib nf (n+1) m
    | otherwise = last fibs
    where nf    = fibs ++ [fibs !! (n-1) + fibs !! (n-2)]

fib :: Int -> Integer
fib = dp_fib [1,1] 2