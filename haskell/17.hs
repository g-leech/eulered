{-
    If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
    If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

    NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) 
    contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. 
    The use of "and" when writing out numbers is in compliance with British usage.
-}

-- Strat: dumbass index manips. recursive.
teens = ["","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen", "nineteen"]
ties = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

wordify x
    | x `elem` [0..19]  = teens !! x
    | x <= 99           = ties !! tens ++ wordify (x `mod` 10)
    | x <= 999          = hundredify x
    | x <= 1000         = "onethousand" 
    | otherwise         = error("can't be bothered") 
    where 
        tens = x `div` 10
        hundreds = x `div` 100
        maybeAnd x = if (x `mod` 100 == 0) then "" else "and"
        hundredify x = wordify hundreds 
                        ++ "hundred" 
                        ++ maybeAnd x 
                        ++ wordify (x `mod` 100)
letterSum xs = sum $ map (length . wordify) xs

main = do 
    let answer = letterSum [1..1000]
    print $ answer