import Data.Ratio

f :: Int -> Ratio Integer
f 0 = 1 % 2
f x = 1 / (2 + f(x-1))

g :: Int -> Ratio Integer
g x = 1 + f x

gen :: Int
gen = length $ filter (\x -> (length . show . numerator $ x) > (length . show . denominator $ x)) $ map g [0..999]

main :: IO ()
main = print gen

{-
➜  euler git:(master) ✗ time ./e.out                                                                                                      Meowcolm
153
./e.out  2.00s user 0.05s system 61% cpu 3.330 total
-}
