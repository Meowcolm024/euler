import Data.Char (digitToInt)
import Data.List (sort)

listToInt :: Num p => [p] -> p
listToInt [] = 0
listToInt (x:xs) = x * 10 ^ (length xs) + listToInt xs

intToList :: Int -> [Int]
intToList = map (digitToInt) . show

choices :: [Int]
choices = [1..999999]

-- >>> valid $ reverse [1..9]
-- True
--
valid :: [Int] -> Bool
valid x = sort x == [1..9]

-- >>> generate 192
-- 192384576
--
generate :: Int -> Int
generate x = listToInt $ go 1 []
    where
        go 1 _ = go 2 (intToList x)
        go n acc | length acc > 9 = []
                 | valid acc = acc
                 | otherwise = go (n+1) (acc++intToList (n*x))

-- >>> results
results :: [Int]
results = reverse . sort $ map generate choices

main :: IO ()
main = print $ head results
