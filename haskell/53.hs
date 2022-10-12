-- How many, not necessarily distinct, values of nChk for 1 <= n <= 100, are nChk > 1 million?

import Utils (fac)

-- Strat 1: Double loop. Memoizes in the background.
-- Strat 2: Combinatorics. Fac is symmetric about n/2

(!) = fac
nChk n k = (!)n `div` denom
    where denom  = (!)k * (!)(n-k)

-- Just need length
chkIsOver n k = nChk n k > 1000000
bools = [ chkIsOver n k | n<-[1..100], k<-[1..n] ]
answer = sum $ map fromEnum bools

main = do
    print $ answer
