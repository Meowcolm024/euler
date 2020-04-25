-- Problem 39
import Data.List

isPs :: Integer -> Bool
isPs n = floor (sqrt (fromIntegral n)) ^ 2 == fromIntegral n

tris :: [(Integer, Integer, Integer)]
tris = [(a, b, floor (sqrt (fromIntegral c))) | a <- [3..400], b <- [a..600],let c = a^2 + b^2, isPs c]

sc (a, b, c) = a + b + c

result :: (Int, Integer)
result = foldr ((\x@(xa, _) acc@(aa, _) -> if xa >= aa then x else acc) . (\x -> (length x, head x))) (0, 0) . group . sort . map sc $ tris

main :: IO ()
main = print result