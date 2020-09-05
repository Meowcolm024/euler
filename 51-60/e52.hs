import Data.List (nub, sort)

contain :: Integer -> String
contain = sort . nub . show

-- >>> sameDigit [251748, 125874]
-- True
--
sameDigit :: [Integer] -> Bool
sameDigit [] = False
sameDigit (x : xs) = and $ map ((contain x ==) . contain) xs

satisfy :: Integer -> Bool
satisfy x = sameDigit [n*x | n <- [1..6]]

ans :: Integer -> Integer
ans x = if satisfy x then x else ans (x+1)

main :: IO ()
main = print $ ans 1
