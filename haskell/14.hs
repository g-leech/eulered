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

m = 999999

collatz 1 = [1]
collatz n
    | even n    = n : (collatz $ n `div` 2)
    | otherwise = n : (collatz $ 3*n + 1)
collatzLen n = length $ collatz n
lensAndInits xs = zip (map collatzLen xs) xs
max' f = last . sortOn f 
initOfMaxCollatz xs = max' fst $ lensAndInits xs
answer = snd $ initOfMaxCollatz [3..m]

-- TODO: obviously parallel problem, should do like 8 at once.

main = do
    print $ collatzLen 13 == 10
    print $ answer