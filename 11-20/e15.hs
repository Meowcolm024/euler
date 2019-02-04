-- Lattice paths

import Data.List
findposition number = (\(Just i)->i) . findIndex (==number)
getposition number  = head . drop number

count :: [(Int, Int)] -> [Int] -> (Int,Int) ->  Int
count ps qs (x,y) 
    | x == 0 || y == 0                 = 1
    | (0 < y || 0 < x) && c1 && c2     = getposition (findposition (x,y-1) ps) qs + getposition (findposition (x-1,y) ps) qs
    | (0 < y || 0 < x) && c1 && not c2 = getposition (findposition (x,y-1) ps) qs + count ps qs (x-1,y)
    | (0 < y || 0 < x) && c2 && not c1 = count ps qs (x,y-1) + getposition (findposition (x-1,y) ps) qs
    | otherwise                        = count ps qs (x-1,y) + count ps qs (x,y-1)
    where c1 = (x,y-1) `elem` ps
          c2 = (x-1,y) `elem` ps

latt :: Int -> Int -> [Int] -> [Int]
latt m n xs
    | n < m     = latt m (n+1) q
    | otherwise = q
    where lp = [(x,y) | x <- [1..n-1], y <- [1..n-1]]
          p  = [(x,y) | x <- [1..n], y <- [1..n]]
          q  = map (count lp xs) p 

main::IO()
main = print (last (latt 20 1 []))
