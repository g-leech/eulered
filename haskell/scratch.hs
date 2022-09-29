import Data.List

problem_47 = find (all ((==4).snd)) . map (take 4) . tails 
                 . zip [1..] . map (length . factors) $ [1..]
fstfac x = [(head a ,length a) | a <- group $ primeFactors x]
fac [(x,y)] = [x^a | a <- [0..y]]
fac (x:xs) = [a*b | a <- fac [x], b <- fac xs]
factors x = fac $ fstfac x
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = [p, m `div` p]
                        | otherwise      = factor m ps
main = do
    print $ problem_47