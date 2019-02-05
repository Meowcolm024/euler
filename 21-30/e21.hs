-- Amicable numbers

getDiv :: Int -> [Int]
getDiv n
    | sq == n   = divs ++ reverse (map (n`div`) (init divs))
    | otherwise = divs ++ reverse (map (n`div`) divs)
    where divs  = [x | x <- [1..floor(sqrt(fromIntegral n))], n `mod` x == 0]
          sq    = floor(sqrt(fromIntegral n)) ^ 2

sumFact :: Int -> Int
sumFact n = sum $ init $ getDiv n

ami = [(x,sumFact x) | x <- [2..10000], let y = sumFact x in sumFact y == x && y /= x && x < y]

main::IO()
main = print (sum (map fst ami) + sum (map snd ami))