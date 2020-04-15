-- Problem 40 Champernowne's constant
import Data.Char (digitToInt)

nums :: String
nums = concatMap show [1..]

ds :: [Int]
ds = [ 10 ^ x - 1 | x <- [1..6]]

result :: Int
result = product [digitToInt $ nums !! x | x <- ds]

main :: IO ()
main = print result
