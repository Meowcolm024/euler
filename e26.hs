-- Reciprocal cycles
-- this problem can be transfer to finding the largest PRIME number under 1,000

isPrime :: Integer -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

lp = [x | x <- [7..1000], isPrime x]

s = map (\n -> head [x | x <- [ceiling(logBase 10 (fromIntegral n))..], 10^x `mod` n == 1]) lp

main::IO()
main = print $ maximum s -- however it only prints the length, just use `elemIndex` to find the d