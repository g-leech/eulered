{-
    UK coinage.
    How many ways to make £2?
-}

-- Strat 1: iterative recursion: downwards from 200p downwards through denoms. 
    -- One recursion per multiple of each denom.
-- Strat 2: Take powerset of `coins`. Define valid predicate. 
    -- Iterate over each set to find the coefficients.

-- algo doesn't work if in desc order
coins = [1,2,5,10,20,50,100,200]

-- tight knot of code, hard to refactor
permute_shrapnel cix target
    | cix == 1  = [[target]]
    | otherwise = concat $ combis
    where
        combis = map append_deeper [0..maxNNextCoin]
            where maxNNextCoin = target `div` newCoin
        newCoin = coins !! newCoinIx
        newCoinIx = cix - 1
        append_deeper k = map (++[k]) $ permute_again
            where 
                newTarget = target - k * newCoin
                permute_again = permute_shrapnel newCoinIx newTarget

-- ix as length = one too many
main = do
    -- print $ permute_shrapnel 2 20 -- make 20p out of 1,2s
    let answer = length $ permute_shrapnel (length coins) 200
    print $ answer