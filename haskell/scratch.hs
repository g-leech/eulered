import Utils (fastprimes)

x :: Int
x = 10 
-- 1 * 10 * 100 * 1000 * 10000 * 100000 * 1000000
log10 x = log x / log 10
diglen x = (floor $ log10 x) + 1

main = do
    print $ fastprimes