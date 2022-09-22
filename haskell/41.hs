{-
    We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
    For example, 2143 is a 4-digit pandigital and is also prime.

    What is the largest n-digit pandigital prime that exists?
-}

-- Start with n = 9 obvs. would be amusing if none of the 300k are prime
-- Strat 1: just test each. 
-- Strat 2: generate permutations betwen 1..9 and 9..1. 
-- Strat 3: generate all n-digits descending, cleverly.


import Utils (perms, isPrime, fromList)

-- don't need the evens
oddify xs = filter (\x -> isEven (last x) ) xs
    where isEven x = x `mod` 2 /= 0
odds x = oddify (perms x)
pandigs x = map fromList (odds x)

check n = filter isPrime $ pandigs cap
    where cap = [n, n-1.. 1]

answer = head $ concatMap check [9,8..1]


main = do
    print $ 2143 `elem` (check 4)
    print $ answer






-- Strat 1: doesn't scale.
-- isPandigital d n
--     | between = take n (sort ds) == [1..n]   
--     | otherwise = False
--     where
--         between = if (n <= l) then (l <= 9) else False
--         ds = toList d
--         l = length $ ds

-- naiveIsPanprime num digits = isPandigital num digits && isPrime num
-- ninePrime x = naiveIsPanprime x 9

-- check n 
--     | isPrime n = n
--     | otherwise = check n-2
-- answer = check $ fromList cap


-- Strat 3: So elegant, but nowhere near enough coverage
cap = [9, 8..1]
floor' = [1..9]

permuteOne el prepended [] = [] 
permuteOne el prepended (x:xs) = [prepended ++ [el] ++ x:xs] ++ l
                            where l = permuteOne el (prepended++[x]) xs
permute el = ls ++ [pre ++ rest ++ [el]] 
    where 
        elIndex = 9 - el + 1
        rest = drop elIndex cap
        pre = take (elIndex-1) cap
        ls = permuteOne el [] $ pre++rest

tops = concatMap permute [7,6..1]
-- map (isPrime.fromList) $ permute 9
-- print $ any' $ map (isPrime.fromList) $ tops