module Year2020.Month06.Day07 where

import qualified Data.Maybe as Maybe

type Name = String

reverseName :: Name -> Name
reverseName name = reverse name

type OptionalName = Maybe Name

anOptionalName :: OptionalName
anOptionalName = Just "Bob"

aMaybeString :: Maybe String
aMaybeString = anOptionalName

withDefaultName :: Name -> OptionalName -> Name
withDefaultName defaultName optionalName =
  Maybe.fromMaybe defaultName optionalName

type Pair a b = (a, b)

stringAndInt :: Pair String Integer
stringAndInt = ("Age", 22)

type FlippedPair b a = (a, b)

flippedStringAndInt :: FlippedPair Integer String
flippedStringAndInt = stringAndInt

