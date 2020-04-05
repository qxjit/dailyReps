module Year2020.Month04.Day05 where

eight :: Int
eight =
  let
    four = 4
  in
    four * 2

nine :: Int
nine =
  let
    three = 3
  in
    three * three

tenLong :: Int
tenLong =
  let
    five = 5
  in
    let
      two = 2
    in
      five * two

tenShort :: Int
tenShort =
  let
    five = 5
    two = 2
  in
    five * two

lambdaEight :: Int
lambdaEight =
  (\four -> four * 2) 4

lambdaTen :: Int
lambdaTen =
  (\five two -> five * two) 5 2

orderedFive :: Int
orderedFive =
  let
    two = 2
    four = two * two
    one = 1
    five = four + one
  in
    five

unorderedFive :: Int
unorderedFive =
  let
    five = four + one
    four = two * two
    two = 2
    one = 1
  in
    five

recursiveLet :: [Int]
recursiveLet =
  let
    allTheOnes = 1 : allTheOnes
  in
    take 1000 allTheOnes

mutuallyRecursiveLet :: [Int]
mutuallyRecursiveLet =
  let
    onesAndZeros = 1 : zerosAndOnes
    zerosAndOnes = 0 : onesAndZeros
  in
    onesAndZeros

letInDo :: Maybe Int
letInDo = do
  one <- Just 1

  let
    two = 2
    three = 3

  four <- Just 4

  Just (one + two + three + four)

