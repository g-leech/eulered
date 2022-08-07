import Utils (isPrime, fromList)
import Control.Monad (replicateM)

canBeCircularPrimeList = [1,3,7,9]


rot n l = y ++ x 
    where (x,y) = splitAt n l
allrots l = map (\x -> rot x l) [0..(length l)-1]
isCircular l =  all (isPrime . fromList) $ allrots l
circular 1 = [[x] | x<-[2,3,5,7]] -- a slightly special case 
circular n = filter isCircular $ replicateM n canBeCircularPrimeList


main = do
    print $ length $ concatMap circular [1..6]
    print $ replicateM 3 canBeCircularPrimeList