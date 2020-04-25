-- Problem 34: Digit factorials
import Data.Char (digitToInt)

fact :: Int -> Int
fact 0 = 1
fact x = product [1..x]

top :: [Int]
top = [3..7 * fact 9] -- max bound

sumfact :: Int -> Int
sumfact = sum . map (fact . digitToInt) . show

isSF :: Int -> Bool
isSF x = x == sumfact x

result :: [Int]
result = filter isSF top

main :: IO ()
main = print . sum $ result
