{-
    Longest Collatz sequence |
    n → n/2 (n is even)
    n → 3n + 1 (n is odd)

    Which starting number, under one million, produces the longest chain?
-}
-- Strat: Haskell memoizes by default
-- Dodgy strat: Does parity help us? Odd inits go UP!!
-- naive is fast enough, 1 sec with -O2

import Data.List (sortOn)

collatz 1 = [1]
collatz n
    | even n    = n : (collatz $ n `div` 2)
    | otherwise = n : (collatz $ 3*n + 1)
lensAndInits xs = zip (map collatzLen xs) xs
    where collatzLen n = length $ collatz n
max' f = last . sortOn f 
initOfMaxCollatz xs = max' fst $ lensAndInits xs

-- TODO: obviously parallel problem, should do like 8 at once.

main = do
    -- print $ collatzLen 13 == 10
    let answer = snd $ initOfMaxCollatz [3..999999]
    print $ answer