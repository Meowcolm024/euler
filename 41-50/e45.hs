-- Problem 45: Triangular, pentagonal, and hexagonal

as :: [Integer]
as = [x*(x+1) `div` 2 | x <- [1..]]

bs :: [Integer]
bs = [x*(2*x-1) | x <- [1..]]

cs :: [Integer]
cs = [x*(3*x-1) `div` 2 | x <- [1..]]

ele :: Integer -> [Integer] -> (Bool, [Integer])
ele _ [] = (False, [])
ele n (x:xs) | n == x = (True, xs)
             | n < x  = (False, x:xs)
             | otherwise = ele n xs

helper :: Integer -> [Integer] -> [Integer] -> (Bool, [Integer], [Integer])
helper n ps qs = (xa && xb, pss, qss)
    where 
        (xa, pss) = ele n ps
        (xb, qss) = ele n qs

calc :: [Integer] -> [Integer] -> [Integer] -> [Integer]
calc [] _ _ = []
calc (z:zs) xs ys = if jd then z : calc zs ps qs else calc zs ps qs
    where
        (jd, ps, qs) = helper z xs ys

result :: [Integer]
result = take 3 $ calc as bs cs

main :: IO ()
main = print result
