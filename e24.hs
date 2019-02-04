-- Lexicographic permutations
-- A better way is using a pencil and a piece of paper
import Data.List
fact n = product [n, n-1 .. 1]
-- from Stack Overflow:https://stackoverflow.com/questions/54512466/project-euler-24-in-haskell/54515258#54515258
-- Thanks Will Ness for fixing the issue
dvm x facty | r == 0    = (d-1, facty)
            | otherwise = (d, r)
                  where
                  (d, r) = divMod x facty

recur :: Int -> Int -> [Int] -> [Int]
recur x y arr
    | y > 0 =  arr !! d : recur r (y-1) (delete (arr !! d) arr)
    | otherwise = arr
    where (d, r) = x `dvm` fact y

main::IO()
main = print (recur 1000000 9 [0..9])