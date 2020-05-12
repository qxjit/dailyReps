module Year2020.Month05.Day12 where

import qualified Control.Monad as Monad

{-
  LIFO laws

  - If (push a q) == Just r
    Then pop r == Just (a, q)

  - peek q == fmap fst (pop q)
-}
class LIFO q where
  push :: a -> q a -> Maybe (q a)
  pop :: q a -> Maybe (a, q a)

  peek :: q a -> Maybe a
  peek = fmap fst . pop

instance LIFO [] where
  push a q =
    Just (a : q)

  pop q =
    case q of
      [] ->
        Nothing

      (a : rest) ->
        Just (a, rest)

instance LIFO Maybe where
  push a q =
    case q of
      Nothing ->
        Just (Just a)

      Just _ ->
        Nothing

  pop q =
    case q of
      Nothing ->
        Nothing

      Just a ->
        Just (a, Nothing)

data SizedQueue a =
  SizedQueue
    { maxSize :: Int
    , currentSize :: Int
    , elements :: [a]
    }

pushSizedQueue :: a -> SizedQueue a -> Maybe (SizedQueue a)
pushSizedQueue a q = do
  Monad.guard (currentSize q < maxSize q)
  pure $
    q
      { currentSize = currentSize q + 1
      , elements = a : elements q
      }

popSizedQueue :: SizedQueue a -> Maybe (a, SizedQueue a)
popSizedQueue q =
  case elements q of
    [] ->
      Nothing

    (a : rest) ->
      let
        remainingQueue =
          q
            { currentSize = currentSize q - 1
            , elements = rest
            }
      in
        Just (a, remainingQueue)

instance LIFO SizedQueue where
  push = pushSizedQueue
  pop = popSizedQueue
