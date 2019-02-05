-- Lexicographic permutations

fib = (map fib' [0..] !!)                 
     where fib' 1 = 1                                                        
           fib' 2 = 1                                                        
           fib' n = fib (n-2) + fib (n-1)

reach :: Integer -> Integer
reach n
    | l = reach (n+1)
    | otherwise = n
    where f = fib $ fromInteger n
          l = f < (10 ^ 999)

main::IO()
main = print $ reach 1