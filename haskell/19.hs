{-
    A leap year occurs on any year divisible by 4, but not on a century unless div by 400.
    How many Sundays fell on the first of the month during the twentieth century 
    (1 Jan 1901 to 31 Dec 2000)?
-}
-- import Data.Time.Calendar (fromGregorian)
-- import Data.Time.Calendar.WeekDate (toWeekDate)

-- Strat 1: libraries like an adult.
-- Call it a baseline.
-- toWeekNum yr mth = weekNum
--                     where 
--                         day = 1
--                         date = fromGregorian yr mth day
--                         (_,_,weekNum) = toWeekDate $ date
-- cheatAnswer = sum [1 | mth <- [1..12],
--                        let years = [1901..2000],
--                        yr <- years, 
--                        let d = toWeekNum yr mth,
--                        let sunday = 7,
--                        d == sunday]


-- Strat 2: torture cyclic data. Zero-indexed, Sunday zero
the_procession = cycle (threeNormYears ++ leapDays) 
                 where 
                    monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
                    -- 1901 is not leap, 1904 is. so begin with 3 normie years
                    threeNormYears = concat $ replicate 3 monthDays
                    leapDays = 31 : 29 : drop 2 monthDays
weekdayify = scanl weekNum initIndex 
    where 
        weekNum x y = (x+y) `mod` 7
        initIndex = 2 -- 1 Jan 1901 = Tuesday
aCenturyOfElvis = take numMonths $ weekdayify the_procession
    where numMonths = 12 * 100

main = do
    let sunday = 0
    let answer = length $ filter (== sunday) aCenturyOfElvis
    print $ answer