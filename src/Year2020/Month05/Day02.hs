module Year2020.Month05.Day02 where

import Data.Ratio ((%))
import qualified Data.Ratio as Ratio

integer42 :: Integer
integer42 = 42

int42 :: Int
int42 = 42

rational42 :: Rational
rational42 = 42

polymorphic42 :: Num a => a
polymorphic42 = 42

rationalFourth :: Rational
rationalFourth = 0.25

doubleFourth :: Double
doubleFourth = 0.25

polymorphicForth :: Fractional a => a
polymorphicForth = 0.25

rationalFourthRatio :: Rational
rationalFourthRatio = 1 % 4

reciprocate :: Rational -> Rational
reciprocate rational =
  Ratio.denominator rational % Ratio.numerator rational

rational42FromInteger :: Rational
rational42FromInteger = toRational integer42

rational42FromInt :: Rational
rational42FromInt = toRational int42

toRationalType :: Real a => a -> Rational
toRationalType = toRational

fromRationalType :: Fractional a => Rational -> a
fromRationalType = fromRational

truncateIntegerRatio :: Integer -> Integer -> Integer
truncateIntegerRatio num denom = truncate (num % denom)

roundIntegerRatio :: Integer -> Integer -> Integer
roundIntegerRatio num denom = round (num % denom)

ceilingIntegerRatio :: Integer -> Integer -> Integer
ceilingIntegerRatio num denom = ceiling (num % denom)

floorIntegerRatio :: Integer -> Integer -> Integer
floorIntegerRatio num denom = ceiling (num % denom)

floatPi :: Float
floatPi = pi

doublePi :: Double
doublePi = pi

polymorphicPi :: Floating a => a
polymorphicPi = pi

