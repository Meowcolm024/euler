
import Data.Char (digitToInt)

-- >>> digitSum 100^100
-- 1
--
digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

ans :: Int
ans = maximum $ map digitSum [a^b | a <- [1..100], b <- [1..100]]

main :: IO ()
main = print ans
