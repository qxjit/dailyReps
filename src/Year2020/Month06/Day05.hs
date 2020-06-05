module Year2020.Month06.Day05 where

eggsLeft1 :: Maybe Integer -> String
eggsLeft1 maybeCarton =
  case maybeCarton of
    Just n | n >= 12 -> "At least a dozen"
    Just n | n == 0 -> "Who left an empty carton"
    Just _ -> "Not even a dozen"
    Nothing -> "I can't find the carton"

myOtherwise :: Bool
myOtherwise = True

eggsLeft2 :: Maybe Integer -> String
eggsLeft2 maybeCarton =
  case maybeCarton of
    Just n
      | n >= 12 -> "At least a dozen"
      | n == 0 -> "Who left an empty carton"
      | otherwise -> "Not even a dozen"

    Nothing ->
      "I can't find the carton"

eggsLeft3 :: Maybe Integer -> String
eggsLeft3 (Just n)
  | n >= 12 = "At least a dozen"
  | n == 0 = "Who left an empty carton"
  | otherwise = "Not even a dozen"

eggsLeft3 Nothing =
  "I can't find the carton"

data Element = Osmium | Gold | Lead

approx :: Rational -> Rational -> Bool
approx target actual =
  abs (target - actual) < 0.1

guessElement1 :: Rational -> Rational -> Maybe Element
guessElement1 g cubicCM
  | approx 22.6 gPerCubicCM = Just Osmium
  | approx 19.3 gPerCubicCM = Just Gold
  | approx 11.4 gPerCubicCM = Just Lead
  | otherwise               = Nothing
  where
    gPerCubicCM = g / cubicCM

guessElement2 :: Rational -> Rational -> Maybe Element
guessElement2 g cubicCM =
  let
    gPerCubicCM = g / cubicCM

    guess
      | approx 22.6 gPerCubicCM = Just Osmium
      | approx 19.3 gPerCubicCM = Just Gold
      | approx 11.4 gPerCubicCM = Just Lead
      | otherwise               = Nothing
  in
    guess

describePitch :: (Integer, Integer) -> String
describePitch xy
  | (0, 0) <- xy = "Right down the middle"
  | (0, y) <- xy, y > 0 = "Up high"
  | (x, y) <- xy, y < 0, x > 0 = "Low and to the right"
  | otherwise = "A wild pitch"

divideOut2s :: Integral n => n -> n
divideOut2s n
  | n == 0 = 0
  | even n, let m = n `div` 2 = divideOut2s m
  | otherwise = n
