{--
    the 5-digit number, 16807=7^5, is also a fifth power. 
    the 9-digit number, 134217728=8^9, is a ninth power.

    How many n-digit positive integers exist which are also an nth power?
--}

-- Strat 1: upper bound the inner loop, expand search again 
-- Powers are a divergent series, 
-- so sum of matches will settle down eventually. TODO: prove.
-- 10^n is never a self-power length. So candidates per iteration are [1..9]

npows n = length $ filter isSelfpower innerLoop
    where 
        -- continue to disdain stringification
        len x = (floor log10) + 1
            where log10 = log x / log 10
        isSelfpower = \x -> len x == n
        innerLoop = map (^n) [1..9]

-- outer loop to find max power
-- just terminate when not increasing
solve acc i 
    | next == 0 = acc
    | otherwise = solve (acc+next) (i+1) 
    where next = npows (i+1)

main = do
    -- print $ len (7^5) == 5
    let answer = solve 0 0
    print $ answer