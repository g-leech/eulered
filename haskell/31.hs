{-
    UK coinage.
    How many ways to make £2?
-}

-- Strat 1: iterative recursion: downwards from 200p while downwards through denoms. 
    -- One recursion per multiple of each denom.
-- Strat 2: Take powerset of `coins`. Define valid predicate. 
    -- Iterate over each set to find the coefficients.


-- doesn't work desc
coins = [1,2,5,10,20,50,100,200]

-- tight knot
permute_shrapnel cix target
    | cix == 1    = [[target]]
    | otherwise = concat $ combis
        where
            combis = map append_deeper [0..maxNOfNextCoin]
            maxNOfNextCoin = target `div` newCoin
            newCoinIx = cix - 1
            newCoin = coins !! newCoinIx
            append_deeper k = map (++[k]) $ permute_again
                where 
                    newTarget = target - k * newCoin
                    permute_again = permute_shrapnel newCoinIx newTarget

-- ix as length = one too many. 
answer = length $ permute_shrapnel (length coins) 200

main = do
    print $ permute_shrapnel 2 20 -- make 20p out of 1,2s
    print $ answer