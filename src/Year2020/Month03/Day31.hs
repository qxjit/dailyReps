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
- Use `elem` to implement isEnglishVowel
- Drop leading values
- Take leading values
- Take leading vowels with leftovers
- Take leading consonants with leftovers

-}
module Year2020.Month03.Day31 where

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
  [ "Veronica", "Victor", "Vanessa", "Vaughn", "Vandy" ]

nameLengths :: [Int]
nameLengths =
  map length names

nameSheet :: String
nameSheet =
  List.intercalate ", " names

totalNameLength :: Int
totalNameLength =
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

isVanessaLong :: Bool
isVanessaLong =
  "Vanessa" `elem` longNames

aLongName :: Maybe String
aLongName =
  List.find isLongName names

shortenedNames :: [String]
shortenedNames =
  map (take 7) names

lostNames :: [String]
lostNames =
  map (drop 7) names

shortenedAndLostNames :: [(String, String)]
shortenedAndLostNames =
  map (splitAt 7) names

isEnglishVowel :: Char -> Bool
isEnglishVowel char =
  char `elem` "AaEeIiOoUu"

dropLeadingVowels :: String -> String
dropLeadingVowels =
  dropWhile isEnglishVowel

takeLeadingVowels :: String -> String
takeLeadingVowels =
  takeWhile isEnglishVowel

takeVowelPrefix :: String -> (String, String)
takeVowelPrefix =
  span isEnglishVowel

takeLeadingConsonants :: String -> (String, String)
takeLeadingConsonants =
  break isEnglishVowel

