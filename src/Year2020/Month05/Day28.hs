module Year2020.Month05.Day28 where

eggsLeft :: Maybe Integer -> String
eggsLeft maybeCarton =
  case maybeCarton of
    Just n | n >= 12 ->
      "At least a dozen"

    Just 0 ->
      "Who left an empty carton"

    Just _ ->
      "Not even a dozen"

    Nothing ->
      "I can't even find the carton"

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
      "I can't even find the carton"


eggsLeft3 :: Maybe Integer -> String
eggsLeft3 (Just n)
  | n >= 12 = "At least a dozen"
  | n == 0 = "Who left an empty carton"
  | otherwise = "Not even a dozen"

eggsLeft3 Nothing =
  "I can't even find the carton"

densityClass :: Rational -> Rational -> String
densityClass g cubicCM
  | gPerCubicCM > 22.61 = "Osmiumly"
  | gPerCubicCM > 19.32 = "Golden"
  | gPerCubicCM > 11.34 = "Leaden"
  | otherwise           = "Helium... ish"
  where
    gPerCubicCM = g / cubicCM

densityClass2 :: Rational -> Rational -> String
densityClass2 g cubicCM =
  let
    gPerCubicCM = g / cubicCM

    result
      | gPerCubicCM > 22.61 = "Osmiumly"
      | gPerCubicCM > 19.32 = "Golden"
      | gPerCubicCM > 11.34 = "Leaden"
      | otherwise           = "Helium... ish"

  in
    result

pitchLocation :: (Int, Int) -> String
pitchLocation xy
  | (0, 0) <- xy = "Right down the middle"
  | (0, y) <- xy, y > 10 = "Up high"
  | (x, y) <- xy, y < 0, x > 0 = "Down and to the right"
  | otherwise = "A wild pitch"

divideOut2s :: Integral n => n -> n
divideOut2s n
  | n == 0 = 0
  | even n, let m = n `div` 2 = divideOut2s m
  | otherwise = n
