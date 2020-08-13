-- Circular primes
import Data.List

rotate :: Eq a => [a] -> [[a]]
rotate []     = []
rotate (x:xs) = (xs ++ [x]) : rot (x:xs) (xs ++ [x]) 
    where
        rot [] _ = []
        rot _ [] = []
        rot p (q:qs)| p == (q:qs) = []
                    | otherwise = (qs ++[q]) : rot p (qs ++[q]) 

ele :: Int -> [Int] -> Bool
ele _ [] = False
ele n (x:xs) | n == x = True
             | n < x  = False
             | otherwise = ele n xs

isPrime n = go 2
  where
    go d
      | d*d > n        = True
      | n `rem` d == 0 = False
      | otherwise      = go (d+1)

primes = filter isPrime [2 .. ]

needed :: [Int]             
needed = takeWhile (< 1000000) primes

isRt :: Int -> Bool
isRt 2 = True
isRt x 
    | any (`elem` "02468") (show x) = False
    | otherwise = all (`ele` primes) xs
    where
        xs = map (\y -> read y :: Int) . rotate . show $ x

helper :: [Int] -> [Int]
helper []     = []        
helper (x:xs) = if isRt x then x : helper (xs \\ init y) else helper xs
    where
        y = map (\y -> read y :: Int) . rotate . show $ x

result :: Int
result = foldr (\x acc -> acc + (length . rotate . show $ x)) 0 (helper needed)

main = print result