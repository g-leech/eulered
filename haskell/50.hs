{-
    The prime 41, can be written as the sum of six consecutive primes:
    41 = 2 + 3 + 5 + 7 + 11 + 13
    This is the longest sum of consecutive primes that adds to a prime below one-hundred.

    Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

-- hidden lines: 13
import Utils (isPrime,maxOn,filterTilFalse)

-- Strat 1: Powerset, but the elements have to be sublists of the original list...
-- Strat 2: generate all runs of prime sums by dropping from the front. 

primesTo n = filter isPrime [1..n] 
ll x = (length x, last x)
cumsum = scanl1 (+)

cappedRunPrimes n = maxOn fst pairs
    where
        pairs = map ll primeRuns
        primeRuns = map genPrimeRunWithout toDrop
        toDrop = [0..3]
        genPrimeRunWithout dropN = filterTilFalse isPrime (sumsUnder dropN)
        sumsUnder dropN = takeWhile (<n) (dropCumsum dropN)
        dropCumsum dropN = cumsum $ drop dropN (primesTo n)

answer = cappedRunPrimes $ 1000000-1

main = do
    print $ (snd $ cappedRunPrimes 100) == 41
    print $ answer 





-- Strat 1: ridiculously slow
-- isSomething = not . isNothing
-- isConsec xs pow = length pow > 1 && isSomething maybeIndex
--     where maybeIndex = rawSubIndex pow xs 
-- runs xs = filter (isConsec xs) (powerset xs) 
-- cappedRunPrimes n = filter (<n) $ sumIsPrime (runs primesTo)
--     where
--         primesTo = filter isPrime [1..n] 
--         sumIsPrime xs = filter (isPrime . sum) xs

