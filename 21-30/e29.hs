-- Distinct powers

import Data.List

nums = length $ nub [x ^ y | x <- [2..100], y <- [2..100]]

main::IO()
main = print nums