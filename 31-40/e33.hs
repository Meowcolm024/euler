import Data.Char (digitToInt)
import Data.List ( (\\), intersect )

data Fr a = Fr a a deriving (Show, Eq)

instance Functor Fr where
  fmap f (Fr x y) = Fr (f x) (f y)

intToList :: Int -> [Int]
intToList = map digitToInt . show

simplify :: Fr Int -> Fr Int
simplify (Fr x y) = let i = gcd x y in Fr (x `div` i) (y `div` i)

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt (x : xs) = x * 10 ^ (length xs) + listToInt xs

valid :: (Fr Int, Fr Int) -> Bool
valid (x, y) = (a == b) && (x /= y) && (x /= ((* 10) <$> y)) && (c `div` d == 0)
  where
    a = simplify x
    b = simplify y
    (Fr c d) = x

reduce :: Fr [Int] -> Fr [Int]
reduce (Fr x y) = let i = intersect x y in Fr (x \\ i) (y \\ i)

nums :: [(Fr Int, Fr Int)]
nums = map (\(x, y) -> (listToInt <$> x, listToInt <$> y)) 
    $ filter (\(_, Fr x y) -> (x /= []) && (x /= [0]) && (y /= [])) 
    $ map (\x -> (intToList <$> x, reduce $ intToList <$> x)) 
    $ [Fr x y | x <- [10 .. 99], y <- [10 .. 99]]

result :: [(Fr Int, Fr Int)]
result = filter valid nums
