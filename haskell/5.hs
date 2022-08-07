{-
    LCM: What is the smallest positive number evenly divisible by all numbers 1 to 20?
-}
import Utils (primes)

{-
    Strats
    1. upper bound: factorial is a cleanNum. (A stupid one.)
    2. Legendre's formula backwards?
    3. Start from 2520 like a bastard
    4. answer has to be even obvs
    **5. Fundamental Theorem**    https://proofwiki.org/wiki/LCM_from_Prime_Decomposition
-}

-- For primes under 20, get the highest power under 20
maxPowersBounded n = [ last [x^a | a <- [1..n], x^a <= n] | 
                        x <- takeWhile (<=n) $ primes ]
-- Product of max powers is the LCM [1..20]
lcm' n = foldl1 (*) (maxPowersBounded n)
answer = lcm' 20

main = do
    print $ lcm' 10 == 2520
    print $ answer 




-- isFactor p n = n `mod` p == 0
-- candidates n hi = [x | x<-[2, 4 ..hi]]
-- dividedAll p n 
--     | all (== True) (map (isFactor p) (candidates n (fac n))) = p
-- cms n = [ dividedAll p n | p<-[1..n] ]
-- lcm n = head (cms n)