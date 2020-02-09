{-# LANGUAGE GADTs #-}
module H20200209 where

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
  (+) <$> AF.liftAp a <*> AF.liftAp b

runBanned :: Tag -> AF.Ap Tagged Int -> Maybe Int
runBanned bannedTag =
  AF.runAp (ban bannedTag)

tags :: AF.Ap Tagged Int -> Set.Set Tag
tags =
  AF.runAp_ extractTag
    where
      extractTag (Tagged tag _) = Set.singleton tag

runIter :: AF.Ap Tagged Int -> Int
runIter =
  AF.iterAp extractValue
    where
      extractValue (Tagged _ a) = a

runBanned2 :: Tag -> AF.Ap Tagged Int -> Maybe Int
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

data Expr1
  = E1_String String
  | E1_Int Int
  | E1_Concat Expr1 Expr1
  | E1_Add Expr1 Expr1

data Expr1Result
  = ResultString String
  | ResultInt Int

evalExpr1 :: Expr1 -> Maybe Expr1Result
evalExpr1 expr =
  case expr of
    E1_String string ->
      pure $ ResultString string

    E1_Int int ->
      pure $ ResultInt int

    E1_Concat a b -> do
      aResult <- evalExpr1 a
      bResult <- evalExpr1 b

      case (aResult, bResult) of
        (ResultString aStr, ResultString bStr) ->
          pure $ ResultString (aStr ++ bStr)

        _ ->
          Nothing

    E1_Add a b -> do
      aResult <- evalExpr1 a
      bResult <- evalExpr1 b

      case (aResult, bResult) of
        (ResultInt aInt, ResultInt bInt) ->
          pure $ ResultInt (aInt + bInt)

        _ ->
          Nothing

data Expr2 a where
  E2_String :: String -> Expr2 String
  E2_Int :: Int -> Expr2 Int
  E2_Concat :: Expr2 String -> Expr2 String -> Expr2 String
  E2_Add :: Expr2 Int -> Expr2 Int -> Expr2 Int

evalExpr2 :: Expr2 a -> a
evalExpr2 expr =
  case expr of
    E2_String string ->
      string

    E2_Int int ->
      int

    E2_Concat a b ->
      evalExpr2 a ++ evalExpr2 b

    E2_Add a b ->
      evalExpr2 a + evalExpr2 b


class FinalStr repr where
  finalString :: String -> repr String
  finalConcat :: repr String -> repr String -> repr String

class FinalInt repr where
  finalInt :: Int -> repr Int
  finalAdd :: repr Int -> repr Int -> repr Int

newtype FinalValue a = FinalValue a
  deriving Show

instance FinalStr FinalValue where
  finalString = FinalValue
  finalConcat (FinalValue a) (FinalValue b) =
    FinalValue (a ++ b)

instance FinalInt FinalValue where
  finalInt = FinalValue
  finalAdd (FinalValue a) (FinalValue b) =
    FinalValue (a + b)

eval :: FinalValue a -> a
eval (FinalValue a) = a

anInt :: Int
anInt =
  eval $
    finalAdd (finalInt 2) (finalAdd (finalInt 10) (finalInt 20))

aString :: String
aString =
  eval $
    finalConcat
      (finalConcat
        (finalString "Hello")
        (finalString " "))
      (finalString "World")
