{-
    Triangle Tn = n(n+1)/2     1, 3, 6, 10, 15, ...
    Pentagonal      Pn=n(3n−1)/2        1, 5, 12, 22, 35, ...
    Hexagonal       Hn=n(2n−1)      1, 6, 15, 28, 45, ...
    It can be verified that T285 = P165 = H143 = 40755.

    Find the next triangle number that is also pentagonal and hexagonal.
-}

-- Strat 1: Reuse nice closed form inverses and just iterate
    -- H = 2n^2 - n
    -- 0 = 2n^2 - n - H
    -- a = 2 =, b = -1, c = -1
    -- 1 + sqrt(1 + 8n) / 4

-- # hidden lines: 5
import Utils (allSame, isInt)


triagonal n = n * (n + 1) `div` 2
pentagonal n = n * (3*n - 1) `div` 2
hexagonal n = n * (2*n - 1)

isPent :: Int -> Bool
isPent n = isInt $ (sqrt(24 * nf + 1) + 1) / 6
    where nf = fromIntegral n
isHex n = isInt $ (sqrt(8 * nf + 1) + 1) / 4
    where nf = fromIntegral n

x = 285
answer = head $ [tri | t<-[x+1..], 
                       let tri=triagonal t, 
                       isPent tri,
                       isHex tri]

main = do
    print $ allSame [hexagonal 143, pentagonal 165, triagonal 285]
    print $ answer 