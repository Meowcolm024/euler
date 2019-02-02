-- Special Pythagorean triplet

getPT :: Int -> [[Int]]
getPT x = [[p^2 - q^2,2*p*q, floor(sqrt(fromIntegral((p^2 - q^2)^2 + (2*p*q)^2)))] | p <- [1..x], q <- [1..x], p > q]

findout :: Int -> [[Int]] -> [Int]
findout n (x:xs)
    | n == sum x = x
    | otherwise = findout n xs

sumPT :: Int -> Int -> [Int]
sumPT n x
    | x `notElem` result = sumPT (n+1) x
    | otherwise   = findout x out
    where out = getPT n
          result = map sum out

outPT :: Int -> [Int]
outPT = sumPT 2

main::IO()
main = do
    let x = product (outPT 1000)
    print x