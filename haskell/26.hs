{-
    Reciprocal cycle lengths

    The decimal representation of the unit fractions
    1/3 =   0.(3)   1/6 =   0.1(6)      1/7 =   0.(142857)     1/9 =   0.(1)
    
    l(1/7) = 6-digit recurring cycle.

    Find the value of denom d < 1000 for which 1/d contains the 
    longest recurring cycle in its decimal fraction part.
-}
-- Strat 1: Good old rem * 10 trick, maybe cycle detection manually
-- Strat 2: ToString then build suffix tree, then get deepest parent node. O(n)
-- Strat 3: extra fr https://mathworld.wolfram.com/MultiplicativeOrder.html
-- Strat 4: Fermat's little theorem

import Utils (maxOn)

{-
    Strat 4
    
    10^d ≡ 1 (mod b)
    L = p | 10^p mod d = 1 
        or
    L ( first `ad` such that ad = 10^d - 1 )
        L(3) = 1 because 3x3 = 9 = 10^1 - 1
        L(7) = 6 because 7x142857 = 999999 = 10^6 - 1
    
    also: coprime to 10 or else terminating
-}
isCoprimeTo10 n = not (isDiv5 n || even n)
    where isDiv5 n = n `rem` 5 == 0 

-- let a terminating number have cyclelen 0 (1 also fine)
-- `head`: min p | d divides (10^p - 1)
cycleLen d
    | isCoprimeTo10 d = head powersOfDivs
    | otherwise = 0
    where
      -- fermat
      flt p = (10^p - 1) `rem` d
      divides p = flt p == 0
      powersOfDivs = filter divides [1..]

main = do
    -- print $ cycleLen 7 == 6
    -- skip evens; they all terminate. 
    let n = 999
    let answer = maxOn cycleLen [1,3..n]
    print $ answer