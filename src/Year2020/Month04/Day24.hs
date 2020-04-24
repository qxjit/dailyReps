module Year2020.Month04.Day24 where

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

instance HasMagnitude [a] where
  magnitude =
    magnitude . length

instance HasMagnitude (Maybe a) where
  magnitude =
    maybe 0 (const 1)

data OrgTree
  = Worker String
  | Manager String [OrgTree]

orgTreeSize :: OrgTree -> Integer
orgTreeSize tree =
  case tree of
    Worker _ ->
      1

    Manager _ subTrees ->
      1 + sum (map orgTreeSize subTrees)

instance HasMagnitude OrgTree where
  magnitude =
    orgTreeSize

nullSized :: HasMagnitude a => a -> Bool
nullSized a =
  magnitude a == 0

compareMagnitude :: HasMagnitude a => a -> a -> Ordering
compareMagnitude a b =
  compare (magnitude a) (magnitude b)

sortNonNullItems :: HasMagnitude a => [a] -> [a]
sortNonNullItems =
  List.sortBy compareMagnitude . filter (not . nullSized)


