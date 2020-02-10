{-# LANGUAGE GADTs #-}
module Reps20200210 where

import qualified Control.Applicative.Free as AF
import qualified Control.Monad as CM
import qualified Data.Set as Set

{--
   Control.Applicative.Free

   - Red / Black tagging type w/ contructors Functor instance
   - ban function
   - use liftAp to build taggedPlus
   - use runAp to implement runBanned
   - use runAp_ to implement tags
   - use iterAp to implement runIter
   - use hoistAp / retractAp to implement runBanned2
--}

data Tag = Red | Black deriving (Show, Eq, Ord)
data Tagged a = Tagged Tag a

instance Functor Tagged where
  fmap f (Tagged tag a) = Tagged tag (f a)

red :: a -> Tagged a
red = Tagged Red

black :: a -> Tagged a
black = Tagged Black

ban :: Tag -> Tagged a -> Maybe a
ban bannedTag (Tagged tag a) = do
  CM.guard (tag /= bannedTag)
  pure a

taggedPlus :: Tagged Int -> Tagged Int -> AF.Ap Tagged Int
taggedPlus a b =
  (+) <$> AF.liftAp a
      <*> AF.liftAp b

runBanned :: Tag -> AF.Ap Tagged a -> Maybe a
runBanned bannedTag =
  AF.runAp (ban bannedTag)

tags :: AF.Ap Tagged a -> Set.Set Tag
tags =
  AF.runAp_ extractTag
    where
      extractTag (Tagged tag _) =
        Set.singleton tag

runIter :: AF.Ap Tagged a -> a
runIter =
  AF.iterAp extractValue
    where
      extractValue (Tagged _ a) =
        a

runBanned2 :: Tag -> AF.Ap Tagged a -> Maybe a
runBanned2 bannedTag =
  AF.retractAp . AF.hoistAp (ban bannedTag)

{--
  DSL Encoding
  (completely different meaning of tagged!)

  Define regular ADT for TaglessInitial encoding of
  String concat / Int addition

  Define result type and evalTaggedInitial

  Use GADT to define TaglessInitial encoding
  Define evalTaglessInitial

  Define typeclass for final encoding of string and int operations
  Define a newtype to implement those classes
  implement Eval for that newtype

  Demonstrate an int and string expression
--}

data InitialExpr
  = InitString String
  | InitInt Int
  | InitReverse InitialExpr
  | InitAdd InitialExpr InitialExpr

data InitialResult
  = ResultString String
  | ResultInt Int

evalInitial :: InitialExpr -> Maybe InitialResult
evalInitial expr =
  case expr of
    InitString string ->
      pure (ResultString string)

    InitInt int ->
      pure (ResultInt int)

    InitReverse subExpr -> do
      result <- evalInitial subExpr
      case result of
        ResultString string ->
          pure (ResultString (reverse string))

        _ ->
          Nothing

    InitAdd exprA exprB -> do
      resultA <- evalInitial exprA
      resultB <- evalInitial exprB
      case (resultA, resultB) of
        (ResultInt a, ResultInt b) ->
          pure (ResultInt (a + b))

        _ ->
          Nothing

data TaglessExpr a where
  TaglessString :: String -> TaglessExpr String
  TaglessInt :: Int -> TaglessExpr Int
  TaglessReverse :: TaglessExpr String -> TaglessExpr String
  TaglessAdd :: TaglessExpr Int -> TaglessExpr Int -> TaglessExpr Int

evalTagless :: TaglessExpr a -> a
evalTagless expr =
  case expr of
    TaglessString string ->
      string

    TaglessInt int ->
      int

    TaglessReverse subExpr ->
      reverse (evalTagless subExpr)

    TaglessAdd exprA exprB ->
      evalTagless exprA + evalTagless exprB


class FinalStr expr where
  finalStr :: String -> expr String
  finalReverse :: expr String -> expr String

class FinalInt expr where
  finalInt :: Int -> expr Int
  finalAdd :: expr Int -> expr Int -> expr Int

newtype FinalValue a = FinalValue a deriving Show

instance FinalStr FinalValue where
  finalStr =
    FinalValue

  finalReverse (FinalValue str) =
    FinalValue (reverse str)

instance FinalInt FinalValue where
  finalInt =
    FinalValue

  finalAdd (FinalValue a) (FinalValue b) =
    FinalValue (a + b)

evalFinal :: FinalValue a -> a
evalFinal (FinalValue a) = a

aString :: String
aString =
  evalFinal $
    finalReverse (finalStr "drloW olleH")

anInt :: Int
anInt =
  evalFinal $
    finalAdd
      (finalAdd (finalInt 2) (finalInt 3))
      (finalInt 10)
