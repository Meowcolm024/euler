-- Factorial digit sum

fact n = product [n, n-1 .. 1]

sd n = [read [x] :: Int | x <- show (fact n)]

main::IO()
main = print (sum $ sd 100)