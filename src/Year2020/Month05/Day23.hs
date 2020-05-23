module Year2020.Month05.Day23 where

describeInt :: Maybe Int -> String
describeInt maybeInt =
  case maybeInt of
    Just n | n > 10 ->
      "Greater than ten!"

    Just _ ->
      "10 or lower!"

    Nothing ->
      "Nothing to see here."

myOtherwise :: Bool
myOtherwise = True

bar :: Either String Int -> String
bar value =
  case value of
    Right n
      | even n -> "An even number."
      | n > 0 -> "A positively odd number"
      | otherwise -> "I'm not positive about this"

    Left s
      | s == reverse s -> "A palindrome."
      | otherwise -> "Just some string."

baz :: String -> String
baz s
  | s == reverse s = "A palindrome."
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

patternMatchit :: Maybe Int -> String
patternMatchit maybeInt
  | Just int <- maybeInt, int > 10 = "Greater than 10"
  | otherwise = "If it's a number, it's not greater than 10"

remove2s :: Int -> Int
remove2s n
  | even n, let m = n `div` 2 = remove2s m
  | otherwise = n

