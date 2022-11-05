{-
    The adversarial combinatorics of poker

    Each line of the file contains ten cards (separated by a single space): 
    the first five are Player 1's cards and the last five are Player 2's cards. 
    You can assume that all hands are valid (no invalid characters or repeated cards), 
    each player's hand is in no specific order, and in each hand there is a clear winner.

    How many hands does Player 1 win?
-}
-- Hidden lines: 23
import Utils (preproc,split,find,sort,dedupe,maxOn,group,digitToInt)

-- longwinded and pedestrian solution, but The Right Way custom types
-- TODO: check nth highest card in case of double tie
type Card = (Int, Char)
type Hand = [Card]
data Ranks = HighCard(Int) | Pair(Int) | 
               TwoPair(Int) | ThreeOfAKind(Int) | 
               Straight(Int) | Flush(Int) | 
               FullHouse(Int) | FourOfAKind(Int) | 
               StraightFlush(Int) | RoyalFlush
               deriving (Eq,Ord)

hand :: [String] -> Hand
hand xs = zip (cardsToInt xs) (suitInt xs) 
    where 
        valToInt x = find x "23456789TJQKA"
        cardsToInt = map (valToInt . head)
        suitInt xs = map (!! 0) $ map tail xs

score :: Hand -> Ranks
score hand
    | isStraight && isFlush = StraightFlush hi
    | isFlush = Flush hi
    | isStraight = Straight hi
    | otherwise = counts vals
    where
        vals = sort $ map fst hand
        hi = maximum vals
        run = [head vals..last vals]
        -- ace can start a low straight:
        isStraight = vals == run || vals == [12,0,1,2,3]
        suits = map snd hand
        isFlush = length (dedupe suits) == 1

counts :: [Int] -> Ranks
counts vals = case numOfVals of {
            [1,4] -> FourOfAKind mode; 
            [2,3] -> FullHouse mode; 
            [1,1,3] -> ThreeOfAKind mode;
            [1,2,2] -> TwoPair mode;
            [1,1,1,2] -> Pair mode;
            otherwise -> HighCard (maximum vals)
        }
        where
            numOfVals = sort $ map length (group vals)
            mode = head . maxOn length $ group vals

players :: FilePath -> ([Hand],[Hand])
players f = (map fst games, map snd games) 
    where
        games = map (splitAt 5) hands 
        list = map (split ' ') $ preproc f
        hands = map hand list

answer :: FilePath -> Int
answer f = length $ filter (==True) oneWins
    where 
        ps = players f
        s1 = loadAndScore fst
        s2 = loadAndScore snd
        loadAndScore func = map score $ func ps
        oneWins = zipWith (>) s1 s2

main :: IO()
main = do
    raw <- readFile "data/p054_poker.txt" 
    print $ answer raw