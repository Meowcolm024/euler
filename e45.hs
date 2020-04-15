-- Problem 45: Triangular, pentagonal, and hexagonal
-- ghc e45.hs -o e.out -O2
-- time ./e.out
-- [1,40755,1533776805]
-- ./e.out  14.89s user 0.13s system 92% cpu 16.159 total

as :: [Integer]
as = [x*(x+1) `div` 2 | x <- [1..]]

bs :: [Integer]
bs = [x*(2*x-1) | x <- [1..]]

cs :: [Integer]
cs = [x*(3*x-1) `div` 2 | x <- [1..]]

ele :: Integer -> [Integer] -> Bool
ele _ [] = False
ele n (x:xs) | n == x = True
             | n < x  = False
             | otherwise = ele n xs

helper :: Integer -> Bool
helper n = ele n bs && ele n cs

result :: [Integer]
result = take 3 . filter helper $ as

main :: IO()
main = print result
