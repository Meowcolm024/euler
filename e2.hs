-- Even Fibonacci numbers

fbci :: Int -> Int
fbci 1 = 1
fbci 2 = 2
fbci x = fbci (x-1) + fbci (x-2)

calFb :: Int -> Int -> Int
calFb s n
    | result <= n = calFb (s+1) n
    | otherwise  = s - 1
    where result = fbci s

maxFb :: Int -> Int
maxFb = calFb 1

main::IO()
main = do
    let x = sum [fbci x | x <- [1..(maxFb 4000000)], even(fbci x)]
    print x