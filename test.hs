-- test functions

dp_fib :: [Integer] -> Int -> Int -> Integer
dp_fib fibs n m
    | n < m     = dp_fib nf (n+1) m
    | otherwise = last fibs
    where nf    = fibs ++ [fibs !! (n-1) + fibs !! (n-2)]

fib :: Int -> Integer
fib = dp_fib [1,1] 2

main::IO()
main = print $ fib 4782