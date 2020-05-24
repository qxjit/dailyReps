module Year2020.Month05.Day24 where

vagueCountDescription :: Maybe Integer -> String
vagueCountDescription maybeCount =
  case maybeCount of
    Just n | n > 10 ->
      "A fair amount"

    Just _ ->
      "Not that many"

    Nothing ->
      "Totally unknown"

myOtherwise :: Bool
myOtherwise = True

specialness :: Either String Integer -> String
specialness value =
  case value of
    Right n
      | even n -> "An even number"
      | n > 0 -> "A positively add number"
      | otherwise -> "A totally unspecial number"

    Left s
      | reverse s == s -> "A palindrome"
      | otherwise -> "Just a string"

specialString :: String -> String
specialString s
  | s == reverse s = "A palindrome"
  | length s > 10 = "A long string"
  | otherwise = "Uninteresting"

densityClass :: Rational -> Rational -> String
densityClass weight volume
  | density > 9.9 = "DENSE"
  | density > 8.8 = "SORTA_DENSE"
  | density > 4.4 = "NOT_SO_DENSE"
  | otherwise = "UNDENSE"
  where
    density = weight / volume

patterns :: Maybe Int -> String
patterns maybeInt
  | Just int <- maybeInt = show int
  | otherwise = "Nothing to show"


patternsAndConditions :: Maybe Int -> String
patternsAndConditions maybeInt
  | Just int <- maybeInt, int > 10 = "Greater than ten"
  | otherwise = "Certainly not a number greater than ten"

remove2s :: Int -> Int
remove2s n
  | even n, let m = n `div` 2 = remove2s m
  | otherwise = n

