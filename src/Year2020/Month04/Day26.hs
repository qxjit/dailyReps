module Year2020.Month04.Day26 where

import qualified Data.List as List

class HasMagnitude a where
  magnitude :: a -> Integer

instance HasMagnitude Integer where
  magnitude =
    id

instance HasMagnitude Int where
  magnitude =
    toInteger

instance HasMagnitude () where
  magnitude =
    const 0

instance HasMagnitude (a, b) where
  magnitude =
    const 2

instance HasMagnitude (a, b, c) where
  magnitude =
    const 3

instance HasMagnitude (Maybe a) where
  magnitude =
    maybe 0 (const 1)

instance HasMagnitude [a] where
  magnitude =
    magnitude . length

data PonziScheme
  = Victim String
  | Fraudster String [PonziScheme]

ponziSchemeMagnitude :: PonziScheme -> Integer
ponziSchemeMagnitude scheme =
  case scheme of
    Victim _ ->
      1

    Fraudster _ victims ->
      sum (1 : map ponziSchemeMagnitude victims)

instance HasMagnitude PonziScheme where
  magnitude =
    ponziSchemeMagnitude

nullSized :: HasMagnitude a => a -> Bool
nullSized a =
  magnitude a == 0

compareMagnitude :: HasMagnitude a => a -> a -> Ordering
compareMagnitude a b =
  compare (magnitude a) (magnitude b)

sortNonNull :: HasMagnitude a => [a] -> [a]
sortNonNull =
  List.sortBy compareMagnitude . filter (not . nullSized)
