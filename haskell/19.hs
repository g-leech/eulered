{-
    A leap year occurs on any year divisible by 4, but not on a century unless div by 400.
    How many Sundays fell on the first of the month during the twentieth century 
    (1 Jan 1901 to 31 Dec 2000)?
-}
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)

-- Strat 1: libraries like an adult.
-- Call it a baseline.
years = [1901..2000]
intsToWeekNum yr mth = x
                    where day = 1
                          (_,_,x) = toWeekDate $ date
                          date = fromGregorian yr mth day
cheatAnswer = sum [1 | mth <- [1..12],
                         yr <- years, 
                         let d = intsToWeekNum yr mth,
                         let sunday = 7,
                         d == sunday]

-- Strat 2: torture cyclic data
-- 1 Jan 1901 = Tuesday. Zero-indexed, Sunday zero
initIndex = 2
sunday = 0
monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
leapDays = 31 : 29 : drop 2 monthDays
numMonths = 12 * 100

-- 1901 is not leap, 1904 is. so begin with 3 normie years
the_procession = cycle (threeNormYears ++ leapDays) 
                 where threeNormYears = concat $ replicate 3 monthDays
week x y = (x+y) `mod` 7
weekdayify xs = scanl week initIndex xs -- getAllWeekdaysOfTheFirstOfTheMonth
aCenturyOfElvis = take numMonths $ weekdayify the_procession
answer = length $ filter (== sunday) aCenturyOfElvis


main = do
    print $ answer


-- isLeap n = (n `mod` 4 == 0) && (n `mod` 100 /= 0 || n `mod` 400 == 0)