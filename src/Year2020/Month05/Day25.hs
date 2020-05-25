module Year2020.Month05.Day25 where

eggsLeft :: Maybe Integer -> String
eggsLeft maybeCount =
  case maybeCount of
    Just n | n >= 12 ->
      "At least a dozen"

    Just _ ->
      "Not even a dozen"

    Nothing ->
      "What are you talking about, I don't even see a carton!"


myOtherwise :: Bool
myOtherwise = True

whatsSpecial :: Either String Integer -> String
whatsSpecial value =
  case value of
    Right n
      | even n -> "It's an even number"
      | n > 0 -> "It's a positively odd number"
      | otherwise -> "A totally unspecial number"

    Left s
      | reverse s == s -> "A palindrome"
      | otherwise -> "Just a string"

stringSpecial :: String -> String
stringSpecial s
  | s == reverse s = "A palindrome"
  | length s > 10 = "A long string"
  | otherwise = "Uninteresting"

densityClass :: Rational -> Rational -> String
densityClass kg cubicM
  | kgPerCubicM > 9.9 = "DENSE"
  | kgPerCubicM > 8.8 = "SORTA_DENSE"
  | kgPerCubicM > 4.4 = "NOT_SO_DENSE"
  | otherwise = "UNDENSE"
  where
    kgPerCubicM = kg / cubicM

patterns :: Maybe Integer -> String
patterns maybeInt
  | Just int <- maybeInt = show int
  | otherwise = "Nothing to show"

patternsAndConditions :: Maybe Integer -> String
patternsAndConditions maybeInt
  | Just int <- maybeInt, int > 10 = "Greater than ten"
  | otherwise = "Not a number or not greater than ten"

divideOut2s :: Integral n => n -> n
divideOut2s n
  | n == 0 = n
  | even n, let m = n `div` 2 = divideOut2s m
  | otherwise = n

