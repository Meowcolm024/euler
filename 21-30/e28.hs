-- Number spiral diagonals

len = 501 -- (1001-1)/2 +1

out = sum (map (\n -> 4 * (n^2) - 6*(n-1)) [2 * x - 1 | x <- [1 .. len]]) - 3

main::IO()
main = print out