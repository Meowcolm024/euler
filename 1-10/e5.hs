-- Smallest multiple

getLCM :: [Int] -> Int
getLCM xs
    | length xs == 2 = lcm (head xs) (last xs) 
    | otherwise      = lcm (head xs) (getLCM (tail xs))

main::IO()
main = do
    let x = getLCM [1..20]
    print x