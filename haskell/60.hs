{-
    3, 7, 109, and 673, are quite remarkable. 
    By taking any two of these primes and concatenating them in any order 
    the result will always be prime. 
    For example, taking 7 and 109, both 7109 and 1097 are prime. 
    The sum of these four primes, 792, represents the lowest sum for a set of 
    four primes with this property.

    Find the lowest sum for a set of five primes for which any two primes 
    concatenate to produce another prime.

    Solution = [a,b,c,d,e] where b isConcatable with a, c isConcatable with b, etc

-}
-- Hidden lines: 18
import Utils (primes,isPrime,all',sort)
import Data.Function (on)
import Data.List (minimumBy)

{-
    Strat 1: n+=2, add n if concatPrime with all before
    Strat 1b: plus backtracking: start over + 2 after upper bound reached
    Strat 2: 4 pairwise intersections: go through each prime, a
        bs = primes concatable with a: 
        cs = bs concatable with b
        ...
-}

-- helper to append two ints. log10 for length-to-shift. 
isConcatPrime a b = isPrime ab && isPrime ba
    where 
        (ab, ba) = (conc a b, conc b a)
        log10 x = log (fromIntegral x) / log 10
        diglen x = (floor $ log10 x) + 1
        conc x y = (x * 10^(diglen y)) + y

{- prevent duplicate sets: drop candidates < current a -}
concatable a xs = filter largeCat xs
    where largeCat x = (x > a) && isConcatPrime a x

{- contraction search! |cs| < |bs| -}
-- TODO: must be some metaprogramming way to do this
-- wiki.haskell.org/A_practical_Template_Haskell_Tutorial
sets limit = [ [a,b,c,d,e] 
               | let ps = takeWhile (< limit) primes,
                      a <- ps, let bs = a `concatable` ps,
                      b <- bs, let cs = b `concatable` bs,
                      c <- cs, let ds = c `concatable` cs,
                      d <- ds, let es = d `concatable` ds,
                      e <- es ]

main = do
    let limit = 9000 -- arbitrary; found by tinkering
    -- absurdly fast if we dishonestly assume 1st is minimal; instead
    let answer limit = sum $ minimumBy (compare `on` sum) 
                        $ sets limit 
    print $ answer limit



-- Strat 1b: Obvious dynamic programming is obvious
-- Incorrect: assumes that first found will be smallest sum
-- findConcats n acc
--     | enoughPrimesFound = acc
--     | n > limit         = findConcats (redo+2) [redo]
--     | allConcatPrime    = findConcats (n+2)  $ acc ++ [n]
--     | otherwise         = findConcats (n+2)    acc    
--     where 
--         enoughPrimesFound = length acc == nPrimes
--         allConcatPrime = all' $ map (isConcatPrime n) acc
--         redo = head acc + 2

-- answer' = findConcats 3 []
