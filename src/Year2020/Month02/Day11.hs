{-# LANGUAGE GADTs #-}
module Year2020.Month02.Day11 where

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

data Tag = Red | Black deriving (Eq, Show, Ord)
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

data Initial
  = InitString String
  | InitInt Int
  | InitConcat Initial Initial
  | InitInc Initial

data InitialResult
  = InitStringResult String
  | InitIntResult Int

evalInitial :: Initial -> Maybe InitialResult
evalInitial expr =
  case expr of
    InitString string ->
      pure $ InitStringResult string

    InitInt int ->
      pure $ InitIntResult int

    InitConcat exprA exprB -> do
      resultA <- evalInitial exprA
      resultB <- evalInitial exprB

      case (resultA, resultB) of
        (InitStringResult stringA, InitStringResult stringB) ->
          pure $ InitStringResult (stringA ++ stringB)

        _ ->
          Nothing

    InitInc subExpr -> do
      subResult <- evalInitial subExpr
      case subResult of
        InitIntResult int ->
          pure $ InitIntResult (int + 1)

        _ ->
          Nothing

data Tagless a where
  TaglessString :: String -> Tagless String
  TaglessInt :: Int -> Tagless Int
  TaglessConcat :: Tagless String -> Tagless String -> Tagless String
  TaglessInc :: Tagless Int -> Tagless Int

evalTagless :: Tagless a -> a
evalTagless expr =
  case expr of
    TaglessString string ->
      string

    TaglessInt int ->
      int

    TaglessConcat a b ->
      evalTagless a ++ evalTagless b

    TaglessInc subExpr ->
      evalTagless subExpr + 1


class FinalStr expr where
  finalStr :: String -> expr String
  finalConcat :: expr String -> expr String -> expr String

class FinalInt expr where
  finalInt :: Int -> expr Int
  finalInc :: expr Int -> expr Int

newtype FinalVal a = FinalVal a

instance FinalStr FinalVal where
  finalStr =
    FinalVal

  finalConcat (FinalVal a) (FinalVal b) =
    FinalVal (a ++ b)

instance FinalInt FinalVal where
  finalInt =
    FinalVal

  finalInc (FinalVal a) =
    FinalVal (a + 1)

evalFinal :: FinalVal a -> a
evalFinal (FinalVal a) =
  a

aStr :: String
aStr =
  evalFinal $
    finalConcat (finalStr "Hello")
                (finalConcat (finalStr " ") (finalStr "World"))
anInt :: Int
anInt =
  evalFinal $
    finalInc (finalInc (finalInc (finalInt 0)))
