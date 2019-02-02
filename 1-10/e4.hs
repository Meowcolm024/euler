-- Largest palindrome product

pali :: Int -> Bool
pali x = let tmp = show x in tmp == reverse tmp

main::IO()
main = do
    let x = maximum [x*y | x <- [100..999], y <- [100..999], pali (x*y)]
    print x