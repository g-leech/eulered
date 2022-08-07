{-
  Fraction puns

  49/98 = 4/8, which is correct, is obtained wrongly by cancelling the 9s.
  consider fractions like 30/50 = 3/5 to be trivial examples.

  There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

  If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
-}
import Data.Ratio ((%))

-- Strat: Use the Rationals operator % to reduce and check equality
-- ab / bc == a / c
-- Validity: Just omit the zeroes and only loop over two digit nums. Get <1 with a /= b


coincidents a b c = ((10*a+b), (10*b+c))
isPun a b c = (ab % bc) == (a % c)
              where (ab, bc) = coincidents a b c

digs = [1..9]
fullanswers = [ (ab, bc) | a<-digs, b<-digs, c<-digs,
         let (ab, bc) = coincidents a b c,
         isPun a b c, 
         a /= b, 
         a /= c
    ]         
answer = product [a%b | (a,b)<-fullanswers]

main = do
    print $ isValid 49 98
    print $ fullanswers
    print $ answer





isValid num den = num < den && 
                  isTwoDigit num && isTwoDigit den &&
                  not ((is10 num) && (is10 den))
isTwoDigit a = a `div` 100 <= 0 
                && a `div` 10 > 0
is10 a = a `mod` 10 == 0
