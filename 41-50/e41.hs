import Data.Char ( digitToInt )
import Data.List (sort)

primes :: [Int]
primes = 2 : filter isPrime [3 ..]

isPrime :: Int -> Bool
isPrime n = foldr (\x acc -> (x*x > n) || (n `rem` x /= 0 && acc)) False primes

possiblePrime :: [Int]
possiblePrime = reverse $ takeWhile (< 10000000) primes

test :: Int -> Bool
test x = 
    let dig = map digitToInt $ show x
    in sort dig == [1.. length dig]

main :: IO ()
main = print . head $ filter test possiblePrime

-- 10000000 => 7652413 accidently get the correct answer!!!!!!!
-- time ./e.out                                                                                                      Meowcolm
-- 7652413
-- ./e.out  3.42s user 0.08s system 75% cpu 4.618 total
