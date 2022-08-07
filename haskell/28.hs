{-
    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13
    sum of the diagonals = 101.

    What is the sum of the diagonals in a 1001 by 1001 spiral?
-}
-- Strat 1: gen the whole thing and access i+1, j+1
-- Strat 2: Iterate over rings and sum corners of each

{-
    Ring 0 = 1      Ring 1 = 24 
    Go counterclockwise and subtract (ring width minus 1 = 2n)
    4n^2 - 12x
-}

-- Assumption: odd width (otherwise nonunique start)
cornersum x 
    | x == 0 = 1
    | otherwise = (tr + tl + bl + br)
    where 
        n = 2*x + 1
        tr = n^2
        tl = n^2 - 2*x
        bl = n^2 - 4*x
        br = n^2 - 6*x

spiral_diagonals a = sum $ map cornersum [0..nRings]
                        where nRings = (a-1) `div` 2

answer = spiral_diagonals 1001

main = do
    print $ spiral_diagonals 5 == 101
    print $ answer
