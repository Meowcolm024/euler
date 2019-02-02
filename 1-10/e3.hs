-- Largest prime factor

isPrime :: Int -> Bool
isPrime k
    | k <= 1    = error "Seriously?"
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

nextPrime :: Int -> Int
nextPrime x
    | isPrime (x+1) = x+1
    | otherwise     = nextPrime (x+1)

factor :: Int -> Int -> Int -> Int
factor l c x
    | out && not finish     = factor c c (x `div` c)
    | not out && not finish = factor l q x
    | otherwise             = c
    where 
        out    = x `mod` c == 0
        finish = c == x
        q      = nextPrime c

getFact :: Int -> Int
getFact = factor 2 2

main::IO()
main = do
    let x = getFact 600851475143
    print x