{-
    The prime 41, can be written as the sum of six consecutive primes:

    41 = 2 + 3 + 5 + 7 + 11 + 13
    This is the longest sum of consecutive primes that adds to a prime below one-hundred.

    The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, 
    and is equal to 953.

    Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

-- hidden lines: 22
import Utils (isPrime,maxOn,map',doUntilFalse)

-- Strat 1: Powerset, but the elements have to be sublists of the original list...
-- Strat 2: 

primesTo n = filter isPrime [1..n] 
ll x = (length x, last x)
cappedRunPrimes n = maxOn fst pairs
    where
        pairs = map ll primeRuns
        primeRuns = map' genPrimeRun [0..10]
        genPrimeRun dropN = doUntilFalse isPrime (sumsUnder dropN)
        sumsUnder x = takeWhile (<n) (cumsumdrop x)
        cumsumdrop dropN = scanl1 (+) $ drop dropN (primesTo $ n-1)


answer = cappedRunPrimes 1000000

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

