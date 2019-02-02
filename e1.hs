-- Multiples of 3 and 5

multiThree :: Int -> Int
multiThree max = let 
                 threes = [x | x <- [3..(max-1)], x `mod` 3 == 0] 
                 fives  = [x | x <- [5..(max-1)], x `mod` 5 == 0]
                 cover  = [x | x <- [15..(max-1)], x `mod` 15 == 0]
                 in sum threes + sum fives - sum cover
                
main::IO()
main = do
    let x = multiThree 1000
    print x