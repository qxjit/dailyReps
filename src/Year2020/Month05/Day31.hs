module Year2020.Month05.Day31 where

eggsLeft1 :: Maybe Integer -> String
eggsLeft1 maybeCarton =
  case maybeCarton of
    Just n | n >= 12 -> "At least a dozen"
    Just 0 -> "Who left an empty carton"
    Just _ -> "Not even a dozen"
    Nothing -> "I can't even find the carton"

myOtherwise :: Bool
myOtherwise = True

eggsLeft2 :: Maybe Integer -> String
eggsLeft2 maybeCarton =
  case maybeCarton of
    Just n
      | n >= 12 -> "At least a dozen"
      | n == 0 -> "Who left an empty carton"
      | otherwise -> "Not even a dozen"

    Nothing -> "I can't even find the carton"

eggsLeft3 :: Maybe Integer -> String
eggsLeft3 (Just n)
  | n >= 12 = "At least a dozen"
  | n == 0 = "Who left an empty carton"
  | otherwise = "Not even a dozen"

eggsLeft3 Nothing =
  "I can't even find the carton"

data Element = Osmium | Gold | Lead

approx :: Rational -> Rational -> Bool
approx target actual =
  abs (target - actual) < 0.1

guessElement1 :: Rational -> Rational -> Maybe Element
guessElement1 g cubicCM
  | approx 22.61 gPerCubicM = Just Osmium
  | approx 19.32 gPerCubicM = Just Gold
  | approx 11.34 gPerCubicM = Just Lead
  | otherwise               = Nothing
  where
    gPerCubicM = g / cubicCM

guessElement2 :: Rational -> Rational -> Maybe Element
guessElement2 g cubicCM =
  let
    gPerCubicM = g / cubicCM

    guess
      | approx 22.61 gPerCubicM = Just Osmium
      | approx 19.32 gPerCubicM = Just Gold
      | approx 11.34 gPerCubicM = Just Lead
      | otherwise               = Nothing
  in
    guess

describePitch :: (Integer, Integer) -> String
describePitch xy
  | (0, 0) <- xy = "Straight down the middle"
  | (0, y) <- xy, y > 10 = "Up high"
  | (x, y) <- xy, y < 0, x > 0 = "Low and to the right"
  | otherwise = "A wild pitch"

divideOut2s :: Integral n => n -> n
divideOut2s n
  | 0 == n = 0
  | even n, let m = n `div` 2 = divideOut2s m
  | otherwise = n
