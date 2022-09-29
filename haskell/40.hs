{-
    Champernowne's constant:
    concat all the naturals, treat as a decimal 0.12... 
    look for particular digits of the result
    
    d_n = nth digit

    d1 * d10 * d100 * d1000 * d10000 * d100000 * d1000000
-}

-- Strat 1: just stringify everything idc
-- Strat 2: construct list up to n, split elems into single digits, and just index
-- Strat 3: cumsum log10 (project into the length dim)
-- Strat 4: construct fractional constant up to n then div and mod
    -- (123456789101112 `div` 10^(l - n)) `mod` 10

-- # hidden lines: 1
import Utils (find)


-- 3: don't bother constructing the constant, just count digits
log10 x = log x / log 10
diglen x = (floor $ log10 x) + 1
cumsum = scanl1 (+)
lens n = cumsum $ map diglen [1..fromIntegral n]

findPivot n = (index1 `div` howManyTimes) `mod` 10
    where
        firstOver n xs = head $ filter (>= n) xs
        ls = lens n
        len = firstOver n ls
        -- bit slower but simpler: search lengths for index of len
        index1 = (find len ls) + 1
        -- how many digits to destroy to clear the way for mod
        howManyTimes = 10^(len - n)

questionIndices = map (10^) [0..5]
mapreduce f xs = foldl1 (*) (map f xs)
answer = mapreduce findPivot questionIndices


main = do
    print $ findPivot 12 == 1
    print $ answer
