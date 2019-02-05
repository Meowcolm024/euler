-- Digit fifth powers
-- 7 * 9 ^5 = 413343, 6 * 9 ^ 5 = 354294, 5 * 9 ^ 5 = 295245 -> just need to check to 354294
limit = 354294

toArr :: Int -> [Int]
toArr xs = [read [x] :: Int | x <- show xs]

fifthProduct :: Int -> Bool
fifthProduct n = sum (map (^ 5) (toArr n)) == n

l = [x | x <- [2..limit], fifthProduct x]

main::IO()
main = print $ sum l