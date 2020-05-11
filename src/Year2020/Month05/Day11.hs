module Year2020.Month05.Day11 where

class LIFO q where
  push :: a -> q a -> q a

  pop :: q a -> Maybe (a, q a)

  peek :: q a -> Maybe a
  peek = fmap fst . pop


instance LIFO [] where
  push = (:)
  pop q =
    case q of
      [] ->
        Nothing

      (a : rest) ->
        Just (a, rest)
