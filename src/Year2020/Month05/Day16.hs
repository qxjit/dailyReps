{-

Write down the laws of our Last in First Out (LIFO) Queue

- Law of Push/Pop
- Law of Peek

Write the LIFO typeclass definition with methods:

- push
- pop
- peek (with a default implementation that follows the law)

Write an instance of LIFO for List

- Implement push
- Implement pop

Write an instance of LIFO for Maybe

- Implement push
- Implement pop

- Define SizedQueue that stores items in a list, but
  has size attributes to enforce a max size

- Implement pushSizedQueue
- Implement popSizedQueue
- Implement peekSizedQueue

- Write an instance of LIFO for SizedQueue

-}
module Year2020.Month05.Day16 where

import qualified Control.Monad as Monad
import qualified Numeric.Natural as Nat

{-
  LIFO Laws

  Law of Push/Pop

  If push a q == Just r
  then pop r == Just (a, q)

  Law of Peek

  peek q == fmap fst (pop q)
-}

class LIFO queue where
  push :: a -> queue a -> Maybe (queue a)

  pop :: queue a -> Maybe (a, queue a)

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

data SizedQueue a =
  SizedQueue
    { maxSize :: Nat.Natural
    , currentSize :: Nat.Natural
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

    a : rest ->
      let
        remainingQueue =
          queue
            { currentSize = currentSize queue - 1
            , items = rest
            }

      in
        Just (a, remainingQueue)

peekSizedQueue :: SizedQueue a -> Maybe a
peekSizedQueue queue =
  case items queue of
    [] ->
      Nothing

    (a : _) ->
      Just a

instance LIFO SizedQueue where
  push = pushSizedQueue
  pop = popSizedQueue
  peek = peekSizedQueue
