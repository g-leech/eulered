{-
    Let d(n) be defined as the sum of divisors of n 
    If d(a) = b and d(b) = a, where a ≠ b, a and b are called amicable numbers.

    Evaluate the sum of all the amicable numbers under 10000.
-}
import Utils (factorPrimes, dedupe, sort)


m = 10000 - 1
powerset [] = [[]]
powerset (x:xs) = [ x:ps | ps<-pow ] ++ pow
                    where pow = powerset xs
divisors n = dedupe . map product . powerset $ factorPrimes n
propaDivisors n = filter (<n) $ divisors n
areAmicable a b sums = (sums !! a == b) && (sums !! b == a)

-- Strat 1: badequate. pairwise test of all 10x10k. O(n^2)
amicables n = [ a+b | 
                a<-[2..n], b<-[3..n-1],
                a < b,
                areAmicable a b divSums ]
              where 
                dsum i = sum (propaDivisors i)
                divSums = 0 : [ dsum i | i <- [1..n]]

answerNaive = sum $ amicables m


main = do
    print $ (sort $ propaDivisors 220) == [1,2,4,5,10,11,20,22,44,55,110]
    let divSums = 0 : [ sum (propaDivisors i) | i <- [1..300]]
    print $ areAmicable 220 284 divSums
    print $ answerNaive