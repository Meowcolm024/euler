-- Summation of primes

isPrime :: Int -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

main::IO()
main = do
    let x = sum[ x | x <- [2..2000000], isPrime x]
    print x