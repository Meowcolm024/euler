-- Large sum

import System.IO

f :: [String] -> [Integer]
f = map read

main = do
    handle <- openFile "e13.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = f singlewords
    let x = show (sum list)
    print (take 10 x)
    hClose handle   
