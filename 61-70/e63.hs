sat :: Int -> Int -> Bool
sat x y =
  let l = length $ show (x ^ y)
   in l == y

-- 9^21 -> 21 digits
-- 9^22 -> 21 digits <- finishes here
result :: [(Int, Int)]
result = filter (uncurry sat) [(i, j) | i <- [0 .. 9], j <- [1 .. 21]]

main :: IO ()
main = print $ length result
