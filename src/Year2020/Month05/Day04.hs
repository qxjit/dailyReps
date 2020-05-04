module Year2020.Month05.Day04 where

import qualified Data.Int as Int

{-

Number hierarchy:

class (Eq a, Show a) => Num a
class (Num a, Ord a) => Real a
class (Real a, Enum a) => Integral a
class (Num a) => Fractional a
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

integerToRational :: Rational
integerToRational = toRational (42 :: Integer)

doubleToRational :: Rational
doubleToRational = toRational (42 :: Double)

realToRational :: Real a => a -> Rational
realToRational = toRational

intToInteger :: Integer
intToInteger = toInteger (42 :: Int)

int16ToInteger :: Integer
int16ToInteger = toInteger (42 :: Int.Int16)

integralToInteger :: Integral a => a -> Integer
integralToInteger = toInteger

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

floatToDouble :: Float -> Double
floatToDouble = realToFrac

myRealToFrac :: (Real a, Fractional b) => a -> b
myRealToFrac = fromRational . toRational

integerFromRoundedDouble :: Integer
integerFromRoundedDouble = round (0.25 :: Double)

intFromCeilingRational :: Int
intFromCeilingRational = ceiling (0.25 :: Rational)

int32FromFlooredFloat :: Int.Int32
int32FromFlooredFloat = floor (0.25 :: Float)

roundType :: (RealFrac a, Integral b) => a -> b
roundType = round

floorType :: (RealFrac a, Integral b) => a -> b
floorType = floor

ceilingType :: (RealFrac a, Integral b) => a -> b
ceilingType = ceiling
