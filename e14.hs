-- Longest Collatz sequence

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

findCs :: Int -> Int -> Int -> Int
findCs m n c
    | c >= d && q     = findCs m (n+1) c
    | c < d && q      = findCs n (n+1) d
    | c >= d && not q = m
    | c < d && not q  = n
    where d = length (chain n)
          l = 1000000
          q = n < l+1

main::IO()
main = print (findCs 1 1 1)

-- need a lot of time, try using a powerful machine