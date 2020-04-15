-- Digit factorials

s = [1..1000]
t = map (\x -> x ^ x) s
u = take 10 (reverse $ show $ sum t)
a = reverse u

main :: IO()
main = print a