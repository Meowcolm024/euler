
import Data.List (permutations, nub)

data Mul a = Mul a a a deriving Show

instance Functor Mul where
    fmap f (Mul x y z) = Mul (f x) (f y) (f z)

listToInt :: Num p => [p] -> p
listToInt [] = 0
listToInt (x:xs) = x * 10 ^ (length xs) + listToInt xs

poss :: [[Int]]
poss = permutations [1..9]

toMul1 :: [[Int]] -> [Mul Int]
toMul1 = map (\xs -> listToInt <$> Mul [head xs] (take 4 $ tail xs) (drop 5 xs))

toMul2 :: [[Int]] -> [Mul Int]
toMul2 = map (\xs -> listToInt <$> Mul (take 2 xs) (take 3 $ drop 2 xs) (drop 5 xs))

check :: Mul Int -> Bool
check (Mul x y z) = x*y == z

result :: Int
result = sum . nub . map (\(Mul _ _ x) -> x) . filter check $ toMul1 poss ++ toMul2 poss

main :: IO ()
main = print result