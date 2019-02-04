-- Counting Sundays

isLeap :: Int -> Bool
isLeap x = (x `mod` 4 == 0 && x `mod` 100 /= 0) || x `mod` 400 == 0
-- Generate a list of Mon~Sun from Jan 1st 1901 to Dec 31st 2000
counts = sum [if isLeap x then 366 else 365 | x <- [1901..2000]]
days = take counts (cycle [1..7])
-- I know this function could be simplified
forYear :: [Integer] -> (Int,Int) -> (Int,Int) -> [[Integer]]
forYear xs (sy,ey) (sm,em)
    | sm `elem` [1,3,5,7,8,10] && con = take 31 xs : forYear (drop 31 xs) (sy,ey) (sm+1,em)
    | sm `elem` [4,6,9,11] && con     = take 30 xs : forYear (drop 30 xs) (sy,ey) (sm+1,em)
    | sm == 2 && con && lp            = take 29 xs : forYear (drop 29 xs) (sy,ey) (sm+1,em)
    | sm == 2 && con && not lp        = take 28 xs : forYear (drop 28 xs) (sy,ey) (sm+1,em)
    | sm == em && sy <= ey            = take 31 xs : forYear (drop 31 xs) (sy+1,ey) (1,em)
    | otherwise                       = []
    where lp = isLeap sy
          con = sy <= ey && sm < em

fyear = forYear days (1901,2000) (1,12)

-- Jan 1st 1900 is mon, so Jan 1st 1901 is actually Tue. Thus 1->Mon and 6->Sun
suns = length(filter (==6) (map head fyear))

main::IO()
main = print suns