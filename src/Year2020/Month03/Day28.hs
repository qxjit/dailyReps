module Year2020.Month03.Day28 where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead as =
  case as of
    [] ->
      Nothing

    (a : _) ->
      Just a

safeTail :: [a] -> Maybe [a]
safeTail as =
  case as of
    [] ->
      Nothing

    (_ : rest) ->
      Just rest

safeLast :: [a] -> Maybe a
safeLast as =
  case as of
    [] ->
      Nothing

    [a] ->
      Just a

    (_ : rest) ->
      safeLast rest

nameLengths :: [Int]
nameLengths =
  map length ["James", "Henrietta", "Judy"]

fullName :: String
fullName =
  List.intercalate " " ["James", "Earl", "Jones"]

total :: Int
total =
  sum [10, 2, 10]

anded :: Bool
anded =
  and [True, True, False]

orred :: Bool
orred =
  or [True, True, False]

isLongName :: String -> Bool
isLongName name =
  length name > 7

anyLongNames :: Bool
anyLongNames =
  any isLongName ["Bob", "George", "Anastasia"]

allLongNames :: Bool
allLongNames =
  all isLongName ["Bob", "George", "Anastasia"]

allTheLongNames :: [String]
allTheLongNames =
  filter isLongName ["Bob", "George", "Anastasia"]

longNames, shortNames :: [String]
(longNames, shortNames) =
  List.partition isLongName ["Bob", "George", "Anastasia"]

isBobLong :: Bool
isBobLong =
  "Bob" `elem` longNames

aLongName :: Maybe String
aLongName =
  List.find isLongName ["Bob", "George", "Anastasia"]

truncateName :: String -> String
truncateName =
  take 7

nameOverflow :: String -> String
nameOverflow =
  drop 7

