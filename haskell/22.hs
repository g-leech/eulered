{-
    begin by sorting it into alphabetical order. 
    Then working out the alphabetical value for each name, 
    multiply this value by its alphabetical position in the list to obtain a name score.

    For example, when the list is sorted into alphabetical order, COLIN, 
    which is worth 3 + 15 + 12 + 9 + 14 = 53, 
    is the 938th name in the list. 
    So, COLIN would obtain a score of 938 × 53 = 49714.

    What is the total of all the name scores in the file?
-}
import Utils (split, sort)
import Data.Char (ord)

disquote = map (filter (/= '"')) 
decsv = map (split ',')
preproc file = disquote . sort . concat . 
                decsv $ lines file
lexico s = map (\x -> ord x - 64) s
alphavals xs = map (sum . lexico) xs
scores xs = zipWith (*) (alphavals xs) [1..]
answer xs = sum $ scores xs


main = do
    raw <- readFile "data/p022_names.txt" 
    print $ answer $ preproc raw
