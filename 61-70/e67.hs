maxTri :: [[Int]] -> Int
maxTri [] = 0
maxTri [x] = maximum x
maxTri (x : y : xs) = maxTri (zipWith max left right : xs)
  where
    left = zipWith (+) (init x) y
    right = zipWith (+) (tail x) y

main :: IO ()
main = do
  contents <- readFile "e67.txt"
  let tmp = map (map read . words) . lines $ contents :: [[Int]]
  print $ maxTri (reverse tmp)