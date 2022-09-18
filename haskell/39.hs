{-
    If p is the perimeter of a right angle triangle with integral length sides a,b,c, 
    there are exactly three solutions for p = 120.
    {20,48,52}, {24,45,51}, {30,40,50}

    For which value of p ≤ 1000, is the number of solutions maximised?
-}

-- Strat 1: gen right triplets and check their sum. Naive n^3 loop 
-- Strat 2: there are only two dof, use em
-- Strat 3: coprimality
-- Strat 4: solve the system
-- Halve the time by doing b from a+1
-- Substrat x. Domain hack: Bigger number, more partitions, more chances
-- Substrat y. Bigger, increasing nonlinear gap, less chance 

import Utils (maxOn)

-- Strat 2. n^2. 4 seconds with O2.
isRight a b c = c^2 == (a^2 + b^2)
triplesWithP p = [ [a, b, c] | a<-[1 ..p], b<-[a+1 ..p],
                                let c = p - a - b, -- a+b+c == p
                                isRight a b c 
                 ]
numSolutions p = length $ triplesWithP p
pairs n = [ (p, numSolutions p) | p<-[1..n] ]  
answer = fst $ maxOn snd (pairs 1000)


main = do
    print $ (numSolutions 120) == 3
    print $ answer




-- argmax (x:xs) n p
--     | (x:xs) == []   = p
--     | n > p    = argmax xs n2 p2
--     | otherwise = argmax xs n p
--     where
--         (p2,n2) = x