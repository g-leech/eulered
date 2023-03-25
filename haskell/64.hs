{--
    the count of the parity of the lengths of the cycles of the partial denominators of the continued fractions of irrational square roots

    All square roots, other than of perfect squares, are irrational
    All square roots are periodic when written as continued fractions
    some of these cycles have odd lengths
    CURIOUS
    
    Exactly four continued fractions N < 14 have an odd period.
    How many continued fractions for N < 10001 have an odd period?
--}

{- 
    Strat 1: Use general cont frac formula 
        https://en.wikipedia.org/wiki/Generalized_continued_fraction
    
    recursively build a list of partial denominators, 
    throw away the convergents

    `num`: a_n 
    `denom`: floor (r - a_n^2 / div b_n-1) 
-}
continu radicand num denom = partialDenom : ps
    where
        -- b_i: #1 digit of root(original radicand), adjusted by previous continuant 
        -- basically floor of multiples 
        partialDenom = (rootTrunk + num) `div` denom
            -- TODO: invariant but recalculated. laziness makes this ok I guess
            where rootTrunk = (truncate . sqrt . fromIntegral) radicand
        ps
            | loopedOnce = []
            | denomNext == 0 = []
            | otherwise = continu radicand numNext denomNext
            where 
                -- we let a_0 = 0; the full fraction is irrational, 
                -- so this will never recur
                -- b_0 = 1 and we're cycling so b_n+1 = 1 too
                loopedOnce = num /= 0 && denom == 1 
                numNext = denom * partialDenom - num
                denomNext = (radicand - numNext^2) `div` denom

numOddPeriodPartialDenomsUpTo n = sum $ map isOddPeriod nonsquares 
    where 
        isOddPeriod r = fromEnum $ odd period 
            -- use initial values of recurrence relation 
            where period = length . tail $ continu r 0 1
        nonsquares = filter (not . isSquare) [2..n]
        isSquare x = root == nearest
            where 
                root = sqrt $ fromIntegral x
                nearest = fromIntegral (round root)

main = do
    -- print $ numOddPeriodPartialDenomsUpTo 13 == 4
    let answer = numOddPeriodPartialDenomsUpTo 10000
    print $ answer 



-- approxRoot r prec = b0 + 1 / (rest bn)
--     where 
--         partialDenoms = map fromIntegral $ continu r 0 1
--         b0 = head partialDenoms
--         bn = concat $ replicate prec (tail partialDenoms)
--         rest [x] = x 
--         rest (x:xs) = x + 1/(rest xs)