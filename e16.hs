-- Power digit sum

arr = [read [x] :: Int | x <- show (2 ^ 1000)]

main::IO()
main = print (sum arr)