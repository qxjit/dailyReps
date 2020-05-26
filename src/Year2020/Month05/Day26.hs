module Year2020.Month05.Day26 where

eggsLeft :: Maybe Integer -> String
eggsLeft maybeCount =
  case maybeCount of
    Just n | n >= 12 ->
      "At least a dozen"

    Just 0 ->
      "Who left an empty carton?"

    Just _ ->
      "Not even a dozen"

    Nothing ->
      "I can't even find the carton"

myOtherwise :: Bool
myOtherwise = True

whatsSpecial :: Either String Integer -> String
whatsSpecial value =
  case value of
    Right n
      | even n -> "It's a even number"
      | n > 0 -> "It's positively odd"
      | otherwise -> "A totally unspecial number"

    Left s
      | s == reverse s -> "A palindrome"
      | otherwise -> "Just a string"


whatsSpecial2 :: Either String Integer -> String
whatsSpecial2 (Right n)
  | even n = "It's an even number"
  | n > 0 = "It's positively odd"
  | otherwise = "A totally unspecial number"

whatsSpecial2 (Left s)
  | s == reverse s = "A palindrome"
  | otherwise = "Just a string"

densityClass :: Rational -> Rational -> String
densityClass kg cubicM
  | kgPerCubicM > 9.9 = "DENSE"
  | kgPerCubicM > 8.8 = "SORTA_DENSE"
  | kgPerCubicM > 4.4 = "NOT_SO_DENSE"
  | otherwise = "UNDENSE"
  where
    kgPerCubicM = kg / cubicM

location :: (Int, Int) -> String
location pair
  | (0, 0) <- pair = "At the origin"
  | otherwise = "Far far away"

betterLocation :: (Int, Int) -> String
betterLocation pair
  | (0, 0) <- pair = "Right over the plate"
  | (0, y) <- pair, y > 10 = "High over the middle"
  | (x, y) <- pair, y < 0, x > 0 = "Down and to the right"
  | otherwise = "A wild pitch"

divideOut2s :: Integral n => n -> n
divideOut2s n
  | n == 0 = n
  | even n, let m = n `div` 2 = divideOut2s m
  | otherwise = n
