-- Highly divisible triangular number

triNum :: Int -> Int
triNum n = (1+n)*n `div` 2

getDiv :: Int -> Int
getDiv n
    | ext = length divs * 2 - 1
    | otherwise = length divs * 2
    where divs = [x | x <- [1..floor(sqrt(fromIntegral n))], n `mod` x == 0]
          sq   = floor(sqrt(fromIntegral n)) ^ 2
          ext  = sq == n

findNum :: Int -> Int
findNum n
    | divs < ext = findNum (n+1)
    | otherwise  = tri
    where tri = triNum n
          divs = getDiv tri
          ext  = 500