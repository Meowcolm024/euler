-- Names scores

import Data.List
import E22data

alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

findPos number = (\(Just i)->i) . findIndex (==number)
getScore x     = 1 + findPos x alpha

s    = map (sum . map getScore) (sort names)
sums = sum $ zipWith (*) s [1..length s]

main::IO()
main = print sums
