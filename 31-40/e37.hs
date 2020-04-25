-- Problem 37
-- ghc e37.hs -o e.out -O2         
-- time ./e.out      
-- ./e.out  186.34s user 1.49s system 96% cpu 3:13.87 total                                                                                            Meowcolm
-- [37,53,73,313,317,373,797,3137,3797,739397]
-- sum : 748294 + 23 = 748317

primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]

ele :: Int -> [Int] -> Bool
ele _ [] = False
ele n (x:xs) | n == x = True
             | n < x  = False
             | otherwise = ele n xs
             
isPr :: Int -> Bool
isPr n = ele n primes

isTrun :: Int -> Bool
isTrun x = istr (show x) && istl (show x)
    where
        istr :: String -> Bool
        istr [] = False
        istr [n] = isPr (read [n] :: Int)
        istr n = let y = (read n :: Int)
                 in not (any (`elem` "2468") n) && (isPr y && istr (tail n))

        istl :: String -> Bool
        istl [] = False
        istl [n] = isPr (read [n] :: Int)
        istl n = let y = (read n :: Int)
                 in not (any (`elem` "2468") n) && (isPr y && istl (init n))

tsts :: [Int]
tsts = drop 4 primes

result :: [Int]
result = take 10 . filter isTrun $ tsts

main :: IO ()
main = print result