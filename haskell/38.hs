{-
    Take the number 192 and multiply it by each of 1, 2, and 3:
    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576
    By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)

    The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

    What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?
-}

import Utils (fromList,sort,toList)

-- Strat 1: upper bound. 
    -- Largest pandigital: 987654321
    -- Largest concatpan will thus start 9[8][7]

-- Elaborate because it handles any subset of [1..9]
isPandig d n
    | between = take n (sort ds) == [1..n]   
    | otherwise  = False
    where
        between = if (n <= l) then (l <= 9) 
                     else False
        ds = toList d
        l = length $ ds

scaleAndList c = toList . (c *)
concProd a n = fromList $ concatMap (scaleAndList a) [1..n] 
concIsPandig a n = isPandig (concProd a n) 9
cap n = 10 ^ (9 `div` n) - 1
-- TODO: check if this is executing concProd twice per loop or gets thunked
getNPandigs n = [ concProd x n | let a = cap n, 
                               x <- [a, a-1 ..1],
                               concIsPandig x n ]
answer = maximum . concat $ map getNPandigs [2..9] 

main = do
    print $ concIsPandig 192 3
    print $ concIsPandig 9 5
    print $ answer
