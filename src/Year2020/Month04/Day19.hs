module Year2020.Month04.Day19 where

import qualified Data.List as List

data USPhoneNumber =
  USPhoneNumber
    { areaCode :: String
    , exchangeCode :: String
    , lineNumber :: String
    }

usPhoneNumberToString :: USPhoneNumber -> String
usPhoneNumberToString phone =
  List.intercalate "-"
    [ areaCode phone
    , exchangeCode phone
    , lineNumber phone
    ]

usPhoneNumberFromString :: String -> USPhoneNumber
usPhoneNumberFromString input =
  let
    (area, dashRest) = List.break (== '-') input
    (exchange, dashLine) = List.break (== '-') (drop 1 dashRest)
  in
    USPhoneNumber
      { areaCode = area
      , exchangeCode = exchange
      , lineNumber = drop 1 dashLine
      }

data EmailAddress =
  EmailAddress
    { username :: String
    , domain :: String
    }

emailAddressToString :: EmailAddress -> String
emailAddressToString email =
  List.intercalate "@"
    [ username email
    , domain email
    ]

emailAddressFromString :: String -> EmailAddress
emailAddressFromString input =
  let
    (user, atDomain) = List.break (== '@') input
  in
    EmailAddress
      { username = user
      , domain = drop 1 atDomain
      }

class StringLike s where
  toString :: s -> String
  fromString :: String -> s

instance StringLike USPhoneNumber where
  toString = usPhoneNumberToString
  fromString = usPhoneNumberFromString

instance StringLike EmailAddress where
  toString = emailAddressToString
  fromString = emailAddressFromString

delimitedToString :: StringLike s => Char -> [s] -> String
delimitedToString delimiter items =
  List.intercalate [delimiter] (map toString items)

delimitedFromString :: StringLike s => Char -> String -> [s]
delimitedFromString delimiter =
  go
    where
      go input =
        case input of
          "" ->
            []

          _ ->
            let
              (piece, delimRest) = List.break (== delimiter) input
            in
              fromString piece : go (drop 1 delimRest)

