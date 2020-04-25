-- Problem 36

pali :: Eq a => [a] -> Bool
pali x = reverse x == x

toBin :: Int -> [Int]
toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

both :: Int -> Bool
both x = pali p && pali q
    where
        p = show x
        q = dropWhile (== 0) (toBin x)

result :: Int
result = sum . filter both $ [1..1000000]

main :: IO ()
main = print result