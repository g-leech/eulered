{-
    root two can be expressed as an infinite continued fraction.
    the eighth expansion 1393/985 is the first with num digits > denom digits
    In 1000 expansions, how many numerators have > digits than their denominator?
-}
import Data.Ratio (approxRational,numerator,denominator)

cap = 1000

-- in THIS house we do NOT use MAGIC numbers
root2 i 
    | i == 0    = 1
    | otherwise = 1 + 1/rec
    where rec = (1 + root2 (i-1))  
rat x = approxRational x 0.01

-- Strat 2: closed form generator plus mutual recursion
{-
    x_n = 1 + \frac{1}{1 + x_{n-1}}
        \frac{1}{1 + p/q}
        \frac{1}{\frac{q}{q} + p/q}
        \frac{1}{\frac{q+p}{q}}
        \frac{q}{q+p}
        \frac{q+p}{q+p} + \frac{q}{q+p}
        \frac{2q+p}{q+p}
-}
rt2_ratios = zip nums dens
    where
        x1 = rat $ root2 1
        (n1, d1) = (numerator x1, denominator x1)
        step n d = n + 2*d
        nums = n1 : zipWith step nums dens
        dens = d1 : zipWith (+) nums dens

len = floor . log . fromIntegral
noseOverTail (n,d) = len n > len d
answer = length $ filter noseOverTail ratios
            where ratios = take cap rt2_ratios

main = do
    print $ answer


-- Strat 1: Compute ~rt2 then infer a ratio pair for each
-- rootrat x = rat (root2 x)
-- f x = log a > log b
--     where 
--         a = fromIntegral (numerator x)
--         b = fromIntegral (denominator x)

-- approxAnswer = length $ filter (==True) isBiggerNums
--     where 
--         ratios = map rootrat [1..1000]
--         isBiggerNums = map f ratios

-- a = \frac{1}{2 + a}
-- a^2 + 2a - 1 = 0
-- a = -1 - \sqrt2 \quad \text{or} \quad   a = \sqrt 2 - 1