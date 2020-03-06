{-# LANGUAGE GADTs #-}
module Year2020.Month02.Day13 where

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

run :: AF.Ap Tagged a -> a
run =
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


data Init
  = InitString String
  | InitChar Char
  | Concat Init Init
  | Cons Init Init

data InitResult
  = IRString String
  | IRChar Char

evalInitial :: Init -> Maybe InitResult
evalInitial expr =
  case expr of
    InitString string ->
      pure $ IRString string

    InitChar char ->
      pure $ IRChar char

    Concat exprA exprB -> do
      resultA <- evalInitial exprA
      resultB <- evalInitial exprB

      case (resultA, resultB) of
        (IRString a, IRString b) ->
          pure $ IRString (a ++ b)

        _ ->
          Nothing

    Cons exprA exprB -> do
      resultA <- evalInitial exprA
      resultB <- evalInitial exprB

      case (resultA, resultB) of
        (IRChar a, IRString b) ->
          pure $ IRString (a : b)

        _ ->
          Nothing

data Tagless a where
  TLString :: String -> Tagless String
  TLChar :: Char -> Tagless Char
  TLConcat :: Tagless String -> Tagless String -> Tagless String
  TLCons :: Tagless Char -> Tagless String -> Tagless String

evalTagless :: Tagless a -> a
evalTagless expr =
  case expr of
    TLString string ->
      string

    TLChar char ->
      char

    TLConcat exprA exprB ->
      evalTagless exprA ++ evalTagless exprB

    TLCons exprA exprB ->
      evalTagless exprA : evalTagless exprB

class FinalStr expr where
  finString :: String -> expr String

class FinalChar expr where
  finChar :: Char -> expr Char

class FinalConcat expr where
  finConcat :: expr String -> expr String -> expr String

class FinalCons expr where
  finCons :: expr Char -> expr String -> expr String

newtype FinalValue a =
  FinalValue { evalFinal :: a }

instance FinalStr FinalValue where
  finString =
    FinalValue

instance FinalChar FinalValue where
  finChar =
    FinalValue

instance FinalConcat FinalValue where
  finConcat a b =
    FinalValue $
      evalFinal a ++ evalFinal b

instance FinalCons FinalValue where
  finCons a b =
    FinalValue $
      evalFinal a : evalFinal b

concatted :: String
concatted =
  evalFinal $
    finConcat (finString "Hello ") (finString "World")

consed :: String
consed =
  evalFinal $
    finCons (finChar 'H') $
      finCons (finChar 'e') $
        finCons (finChar 'l') $
          finString "lo"
