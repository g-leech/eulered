{-
    Ulam spiral
    1 in centre, anticlockwise increments to 49, 
    a square spiral with side length 7 is formed.
    All the primes except 2 are on the diagonals!
    8/13 ≈ 62% of the diagonals are prime.

    If we go to 81, side length 9. And so on.

    What side length is needed to drop the diag prime ratio < 10%?
-}
-- Hidden lines: 12
import Utils (isPrime)

-- Strat 1: naively take n each time, incrementing n. Works fine until <0.13
-- Strat 2: don't recalc, update (weighted) mean
-- Strat 3: just increment the numerator / denominator pair

targetRatio = 0.1
fint = fromIntegral

-- Imperatively, the corners i = cycle [1..4] of an anti spiral
-- are 1 + i*a, a+= 2 at each layer (every 4 corners) (i0 = 4, a0 = 0)
corners = scanl (+) 1 xs
    where xs = concatMap (replicate 4) [2,4..]

-- Strat 3: keep a running total of the numerator and denom
    -- numerator = nPrimes /
    -- denominator = nCorners
diagPrimeRatio (nPrimes,nCorners) candidate 
    -- short-circuit, to avoid 1/4 primality tests
    | topOrLeftAndPrime = (nPrimes+1, nCorners+1)
    | otherwise         = (nPrimes,   nCorners+1)
    where 
        -- Hack: bottom right diag has no primes by definition (perfect squares)
        notOnBottomRight = nCorners `mod` 4 /= 0
        topOrLeftAndPrime = notOnBottomRight && isPrime candidate

nCornersUntilPrimesDrop = last $ takeWhile isOver ratios
    where
        -- store as (numerator, denominator) and traverse with scanl
        -- have to drop first two (numerator 0 -> premature termination)
        ratios = drop 2 $ scanl diagPrimeRatio (0,0) corners
        isOver (n,d) = currentRatio >= targetRatio
            where currentRatio = (fint n) / (fint d)

sideLen nLayers = 2*nLayers + 1
nIterations corners = (corners + 2) `div` 4

answer = nToWidth $ snd nCornersUntilPrimesDrop
    where nToWidth nCorners = sideLen $ nIterations nCorners

main = do 
    print $ answer
    

-- Strat 1: concise but disgustingly slow after a few thou
-- Wastes (x^n) ops
-- count xs = length $ filter (==True) xs
-- numPrimes cs = fint . count $ map isPrime cs
-- diagPrimeRatio' nLayers = np / (fint nc)
--     where
--         cs = take n corners
--         n = nCorners nLayers
--         np = numPrimes cs
--         nc = length cs

-- rec' width
--     | ratio >= targetRatio = rec' (width+1)
--     | otherwise            = sideLen width
--     where 
--         ratio = diagPrimeRatio' width


-- -- Strat 2: take 4 at a time. doesn't actually save any time
-- updateMean mean pastN new = newSum / (n' + 1)
--     where 
--         newSum = mean * n' + new
--         n' = fint pastN

-- diagPrimeRatio cs = np / (fint nc)
--     where
--         np = numPrimes cs
--         nc = length cs
-- nCorners nLayers = 4*nLayers + 1
-- sideLen nLayers = 2*nLayers + 1
-- newCorners n = map (corners !!) [i+1..i+4]
--     where i = nCorners n

-- slidingUpdate ratio nLayers
--     | ratio >= targetRatio = slidingUpdate mu (nLayers+1)
--     | otherwise            = sideLen nLayers
--     where 
--         newRatio = diagPrimeRatio news
--         mu = updateMean ratio oldN newRatio
--         oldN = nCorners nLayers
--         news = newCorners nLayers