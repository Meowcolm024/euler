-- 10001st prime

isPrime :: Int -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

getPrime :: Int -> Int -> Int -> Int
getPrime last this max
    | prime && not reach = getPrime this (this+1) (max-1)
    | prime && reach = this
    | otherwise = getPrime last (this+1) max
    where prime = isPrime this
          reach = max == 0

truePrime :: Int -> Int
truePrime 1 = 2
truePrime 2 = 3
truePrime k = getPrime 2 3 (k-2)

main::IO()
main = do
    let x = truePrime 10001
    print x