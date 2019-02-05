-- Quadratic primes
import Data.List

isPrime :: Integer -> Bool
isPrime k
    | k <= 1    = False
    | otherwise = null [ x | x <- [2..floor(sqrt(fromIntegral k))], k `mod` x == 0]

pdt :: (Integer,Integer) -> Integer
pdt (x,y) = x*y

getElem x = (\(Just i)->i) . elemIndex x

as = [-999..999]
bs = [x | x <- [-1000..1000], isPrime (abs x)]
ab = [(x,y) | x <- as, y <- bs]
counts = map (\(a,b) -> length (takeWhile isPrime [x^2 + a*x + b | x <- [0..]])) ab

main::IO()
main = print $ pdt $ ab !! getElem (maximum counts) counts