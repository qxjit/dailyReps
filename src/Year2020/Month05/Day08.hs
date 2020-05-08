module Year2020.Month05.Day08 where

import qualified Data.Int as Int
{-
  Numeric Hierarchy

  class (Show a, Eq a) => Num a
  class (Num a, Ord a) => Real a
  class (Real a, Enum a) => Integral a
  class (Num a) => Fractional a
  class (Fractional a, Real a) => RealFrac a
  class (Fractional a) => Floating a
-}

integer42 :: Integer
integer42 = 42

rational42 :: Rational
rational42 = 42

num42 :: Num a => a
num42 = 42

rationalFromInteger :: Rational
rationalFromInteger = fromInteger 42

numFromInteger :: Num a => Integer -> a
numFromInteger = fromInteger

summed42 :: Num a => a
summed42 = 21 + 21

multiplied42 :: Num a => a
multiplied42 = 2 * 21

integerToRational :: Rational
integerToRational = toRational (42 :: Integer)

doubleToRational :: Rational
doubleToRational = toRational (0.25 :: Double)

realToRational :: Real a => a -> Rational
realToRational = toRational

intToInteger :: Integer
intToInteger = toInteger (42 :: Int)

int16ToInteger :: Integer
int16ToInteger = toInteger (42 :: Int.Int16)

integralToInteger :: Integral a => a -> Integer
integralToInteger = toInteger

divved42 :: Integral a => a
divved42 = 84 `div` 2

int32ToInt64 :: Int.Int64
int32ToInt64 = fromIntegral (42 :: Int.Int32)

myFromIntegral :: (Integral a, Num b) => a -> b
myFromIntegral = fromInteger . toInteger

rationalFourth :: Rational
rationalFourth = 0.25

doubleFourth :: Double
doubleFourth = 0.25

fractionalFourth :: Fractional a => a
fractionalFourth = 0.25

doubleFromRational :: Double
doubleFromRational = fromRational 0.25

fractionalFromRational :: Fractional a => Rational -> a
fractionalFromRational = fromRational

quotientQuarter :: Fractional a => a
quotientQuarter = 1 / 4

floatToDouble :: Float -> Double
floatToDouble = realToFrac

myRealToFrac :: (Real a, Fractional b) => a -> b
myRealToFrac = fromRational . toRational

roundDoubleToInteger :: Integer
roundDoubleToInteger = round (0.25 :: Double)

ceilingRationalToInt :: Int
ceilingRationalToInt = ceiling (0.25 :: Rational)

floorFloatToInt32 :: Int.Int32
floorFloatToInt32 = floor (0.25 :: Float)

roundType :: (RealFrac a, Integral b) => a -> b
roundType = round

ceilingType :: (RealFrac a, Integral b) => a -> b
ceilingType = ceiling

floorType :: (RealFrac a, Integral b) => a -> b
floorType = floor

squareRootedQuarter :: Floating a => a
squareRootedQuarter = sqrt (0.25 * 0.25)
