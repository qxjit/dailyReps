{-

- safeHead
- safeTail
- safeLast
- listOfNames
- Use map to find length of names
- Use intercalate to join list of names
- Use sum to total of the length of names
- Function to determine if name is long
- Use `and` to find if all names are long
- Use `all` to find if all names are long
- Use `or` to find if any names are long
- Use `any` to find if any names are long
- Filter out all the long names
- partition long and short nomes
- Check if a particular name is elem of long names
- `find` a long name
- truncate the names so they're all short
- `drop` the truncated parts to find the leftover
- `splitAt` to find the truncated parts and leftover parts at once

-}
module Year2020.Month03.Day29 where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead as =
  case as of
    [] ->
      Nothing

    a : _ ->
      Just a

safeTail :: [a] -> Maybe [a]
safeTail as =
  case as of
    [] ->
      Nothing

    _ : rest ->
      Just rest

safeLast :: [a] -> Maybe a
safeLast as =
  case as of
    [] ->
      Nothing

    [a] ->
      Just a

    _ : rest ->
      safeLast rest

names :: [String]
names =
  [ "Bob", "Anastasia", "Jerrod", "Tina", "Sebastian" ]

nameLengths :: [Int]
nameLengths =
  map length names

joinedNames :: String
joinedNames =
  List.intercalate ", " names

totalLength :: Int
totalLength =
  sum nameLengths

isLongName :: String -> Bool
isLongName name =
  length name > 7

allNamesAreLong1 :: Bool
allNamesAreLong1 =
  and (map isLongName names)

allNamesAreLong2 :: Bool
allNamesAreLong2 =
  all isLongName names

anyNamesAreLong1 :: Bool
anyNamesAreLong1 =
  or (map isLongName names)

anyNamesAreLong2 :: Bool
anyNamesAreLong2 =
  any isLongName names

justLongNames :: [String]
justLongNames =
  filter isLongName names

longNames, shortNames :: [String]
(longNames, shortNames) =
  List.partition isLongName names

isBobLongElem :: Bool
isBobLongElem =
  "Bob" `elem` longNames

aLongName :: Maybe String
aLongName =
  List.find isLongName names

truncatedNames :: [String]
truncatedNames =
  map (take 7) names

nameOverflows :: [String]
nameOverflows =
  map (drop 7) names

namesAndOverflows :: [(String, String)]
namesAndOverflows =
  map (splitAt 7) names

