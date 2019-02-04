-- Lexicographic permutations
import Data.MemoCombinators.Class (memoize)

fib = memoize fib'
    where 
    fib' :: Integer -> Integer
    fib' 0 = 0
    fib' 1 = 1
    fib' n = fib (n-1) + fib (n-2)

reach :: Integer -> Integer
reach n
    | l = reach (n+1)
    | otherwise = n
    where f = fib $ fromInteger n
          l = f < (10 ^ 999)