{-
    The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
    Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-}

-- Strat 1: explicit one-liner, mod 10^10
-- Strat 2: modular exponentiation with bitshifting

x = 10
m = 1000

nton n = n^n 
answer = sumSelfPowers `mod` 10^x
    where sumSelfPowers = sum $ map nton [1..m]


main = do
    print $ answer
