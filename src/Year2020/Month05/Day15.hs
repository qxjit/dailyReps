module Year2020.Month05.Day15 where

import qualified Control.Monad as Monad

{-
  LIFO laws

  Law of pop:

  If push a q == Just r
  then pop r = Just (a, q)

  Law of peek:

  peek q == fmop fst (pop q)

-}

class LIFO queue where

  push :: a -> queue a -> Maybe (queue a)
  pop :: queue a -> Maybe (a, queue a)

  peek :: queue a -> Maybe a
  peek = fmap fst . pop


instance LIFO [] where
  push a queue =
    Just (a : queue)

  pop queue =
    case queue of
      [] ->
        Nothing

      a : rest ->
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
      Nothing ->
        Nothing

      Just a ->
        Just (a, Nothing)

data SizedQueue a =
  SizedQueue
    { maxSize :: Integer
    , currentSize :: Integer
    , items :: [a]
    }

pushSizedQueue :: a -> SizedQueue a -> Maybe (SizedQueue a)
pushSizedQueue a queue = do
  Monad.guard (currentSize queue < maxSize queue)
  pure $
    queue
      { currentSize = currentSize queue + 1
      , items = a : items queue
      }

popSizedQueue :: SizedQueue a -> Maybe (a, SizedQueue a)
popSizedQueue queue =
  case items queue of
    [] ->
      Nothing

    (a : rest) ->
      let
        remainingQueue =
          queue
            { currentSize = currentSize queue - 1
            , items = rest
            }

      in
        Just (a, remainingQueue)

peekSizedQueue :: SizedQueue a -> Maybe a
peekSizedQueue =
  peek . items

instance LIFO SizedQueue where
  push = pushSizedQueue
  pop = popSizedQueue
  peek = peekSizedQueue
