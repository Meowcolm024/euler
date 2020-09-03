import Data.List

primes :: [Int]
primes = filter isPrime [2 ..]

isPrime :: Int -> Bool
isPrime n = go 2
  where
    go d
      | d * d > n = True
      | n `rem` d == 0 = False
      | otherwise = go (d + 1)

needed :: [Int]
needed = takeWhile (< 1000000) primes

conse :: Int -> Int -> [Int] -> [(Int, Int)] -> [(Int, Int)]
conse _ _ [] ys = ys
conse c _ xs [] = conse c 0 xs [(1, 0)]
conse acc i (x : xs) ys
  | c > 1000000 = ys
  | isPrime c = conse c (i+1) xs ((c, i + 1) : ys)
  | otherwise = conse c (i+1) xs ys
  where
    c = acc + x

cons :: [Int] -> [(Int, Int)]
cons [] = []
cons xs = conse 0 0 xs [] ++ cons (tail xs)

result :: (Int, Int)
result = head $ sortBy (\(_,c) (_,d) -> compare d c) $ cons needed

main :: IO ()
main = print result

{- 
(997651,543)
./e.out  1.38s user 0.03s system 73% cpu 1.903 total 
-}