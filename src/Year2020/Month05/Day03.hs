module Year2020.Month05.Day03 where

import Data.Int (Int32, Int64)
import Data.Ratio ((%))
import qualified Data.Ratio as Ratio

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

integerFromInt :: Integer
integerFromInt = toInteger (42 :: Int)

integerFromIntegral :: Integral a => a -> Integer
integerFromIntegral = toInteger

int64From32 :: Int64
int64From32 = fromIntegral (42 :: Int32)

myFromIntegral :: (Integral a, Num b) => a -> b
myFromIntegral = fromIntegral . toInteger

rationalQuarter :: Rational
rationalQuarter = 0.25

doubleQuarter :: Double
doubleQuarter = 0.25

fractionalQuarter :: Fractional a => a
fractionalQuarter = 0.25

rationalFromInt :: Rational
rationalFromInt = toRational (42 :: Integer)

rationalFromDouble :: Rational
rationalFromDouble = toRational (0.25 :: Double)

rationalFromReal :: Real a => a -> Rational
rationalFromReal = toRational

rationalByConstruction :: Rational
rationalByConstruction = 1 % 4

reciprocal :: Rational -> Rational
reciprocal rat = Ratio.denominator rat % Ratio.numerator rat

doubleFromRational :: Double
doubleFromRational = fromRational 0.25

fractionalFromRational :: Fractional a => Rational -> a
fractionalFromRational = fromRational

doubleFromFloat :: Double
doubleFromFloat = realToFrac (0.25 :: Double)

myRealToFrac :: (Real a, Fractional b) => a -> b
myRealToFrac = fromRational . toRational

roundDoubleToInteger :: Double -> Integer
roundDoubleToInteger = round

ceilingRationalToInt :: Rational -> Int
ceilingRationalToInt = ceiling

floorFloatToInt32 :: Float -> Int32
floorFloatToInt32 = floor

roundType :: (RealFrac a, Integral b) => a -> b
roundType = round

floorType :: (RealFrac a, Integral b) => a -> b
floorType = floor

ceilingType :: (RealFrac a, Integral b) => a -> b
ceilingType = ceiling


