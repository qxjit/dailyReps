module Year2020.Month05.Day27 where

eggsLeft :: Maybe Integer -> String
eggsLeft maybeCarton =
  case maybeCarton of
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
      | even n -> "It's an even number"
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

densityClass2 :: Rational -> Rational -> String
densityClass2 kg cubicM =
  let
    kgPerCubicM = kg / cubicM

    result
      | kgPerCubicM > 9.9 = "DENSE"
      | kgPerCubicM > 8.8 = "SORTA_DENSE"
      | kgPerCubicM > 4.4 = "NOT_SO_DENSE"
      | otherwise = "UNDENSE"

  in
    result

pitchLocation :: (Int, Int) -> String
pitchLocation xy
  | (0, 0) <- xy = "Right down the middle"
  | (0, y) <- xy, y > 10 = "Up hight"
  | (x, y) <- xy, y < 0, x > 0 = "Down and to the right"
  | otherwise = "A wild pitch"

