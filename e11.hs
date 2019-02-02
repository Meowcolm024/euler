-- Largest product in a grid

nums = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
arr = [read [x] :: Int | x <- nums, x /= ' ']

com :: [Int] -> [Int]
com xs
    | length xs >= 2 = (head p*10 + last p):com (drop 2 xs)
    | otherwise      = []
    where p = take 2 xs

cmp :: [[Int]] -> [[Int]]
cmp xs 
    | t > 0     = map head xs:cmp (map tail xs)
    | otherwise = []
    where t = length (head xs)

rightMat :: [Int] -> [[Int]]
rightMat xs
    | length xs > 20 = take 20 xs:rightMat(drop 20 xs)
    | otherwise      = [xs]
-- flip the matrix
downMat :: [[Int]] -> [[Int]]
downMat xs
    | l > 0     = map head xs:downMat (map (drop 1) xs)
    | otherwise = []
    where l = length (head xs) 

mat1 = rightMat (com arr)
mat2 = downMat mat1

getRight :: Int -> [[Int]] -> Int
getRight m xs
    | s > m && l > 4 = getRight s (map (drop 1) xs)
    | s < m && l > 4 = getRight m (map (drop 1) xs)
    | otherwise      = max s m
    where t = map (take 4) xs
          s = maximum (map product t)
          l = length (head xs)

getDia :: Int -> [[Int]] -> Int
getDia m xc
    | s > m && t > 4 = getDia s (drop 1 xc)
    | s < m && t > 4 = getDia m (drop 1 xc)
    | otherwise      = max s m
    where l1 = take 16 (head xc)
          l2 = take 16 (drop 1 (last(take 2 xc)))
          l3 = take 16 (drop 2 (last(take 3 xc)))
          l4 = take 16 (drop 3 (last(take 4 xc)))
          l5 = take 16 (last(take 4 xc))
          l6 = take 16 (drop 1 (last(take 3 xc)))
          l7 = take 16 (drop 2 (last(take 2 xc)))
          l8 = take 16 (drop 3 (head xc))
          s1 = maximum (map product (cmp [l1,l2,l3,l4]))
          s2 = maximum (map product (cmp [l5,l6,l7,l8]))
          s  = max s1 s2
          t  = length xc
          
main::IO()
main = do
    let x = getRight 0 mat1
    let y = getRight 0 mat2
    let z = getDia 0 mat1
    print (maximum [x,y,z])

-- takes me over 2 hr QAQ