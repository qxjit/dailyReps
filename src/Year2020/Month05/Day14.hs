module Year2020.Month05.Day14 where

import qualified Control.Monad as Monad

{-
  LIFO Laws

  - Last in first out:

    If push a q == Just r
    then pop r = Just (a, q)

  - Peek:
    peek q == fmap fst (pop q)
-}

class LIFO queue where
  push :: a -> queue a -> Maybe (queue a)

  pop :: queue a -> Maybe (a, (queue a))

  peek :: queue a -> Maybe a
  peek =
    fmap fst . pop

instance LIFO [] where
  push a queue =
    Just (a : queue)

  pop queue =
    case queue of
      [] ->
        Nothing

      (a : rest) ->
        Just (a, rest)

instance LIFO Maybe where
  push a queue =
    case queue of
      Nothing ->
        Just (Just a)

      Just _ ->
        Nothing

  pop queue =
    case queue of
      Just a ->
        Just (a, Nothing)

      Nothing ->
        Nothing

data LimitQueue a =
  LimitQueue
    { maxSize :: Integer
    , currentSize :: Integer
    , items :: [a]
    }

pushLimitQueue :: a -> LimitQueue a -> Maybe (LimitQueue a)
pushLimitQueue a queue = do
  Monad.guard (currentSize queue < maxSize queue)
  pure $
    queue
      { currentSize = currentSize queue + 1
      , items = a : items queue
      }

popLimitQueue :: LimitQueue a -> Maybe (a, LimitQueue a)
popLimitQueue queue =
  case items queue of
    [] ->
      Nothing

    (a : rest) ->
      let
        newQueue =
          queue
            { currentSize = currentSize queue - 1
            , items = rest
            }
      in
        Just (a, newQueue)

peekLimitQueue :: LimitQueue a -> Maybe a
peekLimitQueue =
  peek . items

instance LIFO LimitQueue where
  push = pushLimitQueue
  pop = popLimitQueue
  peek = peekLimitQueue
