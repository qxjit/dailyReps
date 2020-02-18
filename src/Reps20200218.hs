{-# LANGUAGE GADTs #-}
module Reps20200218 where

import qualified Control.Monad as CM
import qualified Control.Applicative.Free as AF
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

data Tag
  = Red
  | Black
  deriving (Show, Eq, Ord)

data Tagged a
  = Tagged Tag a

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
  (+) <$> AF.liftAp a <*> AF.liftAp b

runBanned :: Tag -> AF.Ap Tagged a -> Maybe a
runBanned bannedTag =
  AF.runAp (ban bannedTag)

tags :: AF.Ap Tagged a -> Set.Set Tag
tags =
  AF.runAp_ extractTag
    where
      extractTag (Tagged tag _) =
        Set.singleton tag

runTagless :: AF.Ap Tagged a -> a
runTagless =
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

data Initial
  = InitStr String
  | InitInt Int
  | InitLength Initial
  | InitPlus Initial Initial

data InitialResult
  = InitResString String
  | InitResInt Int

evalInitial :: Initial -> Maybe InitialResult
evalInitial expr =
  case expr of
    InitStr string ->
      pure $ InitResString string

    InitInt int ->
      pure $ InitResInt int

    InitLength subExpr -> do
      result <- evalInitial subExpr
      case result of
        InitResString string ->
          pure $ InitResInt (length string)

        _ ->
          Nothing

    InitPlus exprA exprB -> do
      resultA <- evalInitial exprA
      resultB <- evalInitial exprB
      case (resultA, resultB) of
        (InitResInt intA, InitResInt intB) ->
          pure $ InitResInt (intA + intB)

        _ ->
          Nothing

data Tagless a where
  TaglessStr :: String -> Tagless String
  TaglessInt :: Int -> Tagless Int
  TaglessLength :: Tagless String -> Tagless Int
  TaglessPlus :: Tagless Int -> Tagless Int -> Tagless Int

evalTagless :: Tagless a -> a
evalTagless expr =
  case expr of
    TaglessStr string ->
      string

    TaglessInt int ->
      int

    TaglessLength subExpr ->
      length (evalTagless subExpr)

    TaglessPlus exprA exprB ->
      evalTagless exprA + evalTagless exprB

class FinalStr repr where
  finalStr :: String -> repr String

class FinalInt repr where
  finalInt :: Int -> repr Int

class FinalLength repr where
  finalLength :: repr String -> repr Int

class FinalPlus repr where
  finalPlus :: repr Int -> repr Int -> repr Int

newtype FinalVal a
  = FinalVal { evalFinal :: a } deriving Show

instance FinalStr FinalVal where
  finalStr = FinalVal

instance FinalInt FinalVal where
  finalInt = FinalVal

instance FinalLength FinalVal where
  finalLength (FinalVal string) =
    FinalVal (length string)

instance FinalPlus FinalVal where
  finalPlus (FinalVal a) (FinalVal b) =
    FinalVal (a + b)

aStr :: String
aStr =
  evalFinal $
    finalStr "Hello"

anInt :: Int
anInt =
  evalFinal $
    finalPlus
      (finalInt 1)
      (finalPlus
        (finalLength (finalStr "Hello"))
        (finalLength (finalStr "World")))
