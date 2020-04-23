module Year2020.Month04.Day23 where

data Rect =
  Rect
    { rectWidth :: Double
    , rectHeight :: Double
    }

rectArea :: Rect -> Double
rectArea rect =
  rectWidth rect * rectHeight rect

data Circle =
  Circle
    { circleRadius :: Double
    }

circleArea :: Circle -> Double
circleArea circle =
  let
    r = circleRadius circle
  in
    pi * r * r

class Shape s where
  shapeArea :: s -> Double

instance Shape Rect where
  shapeArea = rectArea

instance Shape Circle where
  shapeArea = circleArea

totalPaintCost :: Shape s => Double -> s -> Double
totalPaintCost costPerArea shape =
  costPerArea * shapeArea shape


