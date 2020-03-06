{-# LANGUAGE GADTs #-}
module Year2020.Month02.Day08 where

import qualified Control.Applicative.Free as AF
import qualified Data.Set as Set

{--
   Control.Applicative.Free
--}

data Tag = Red | Black deriving (Show, Eq, Ord)
data Tagged a = Tagged Tag a

red :: a -> Tagged a
red = Tagged Red

black :: a -> Tagged a
black = Tagged Black

instance Functor Tagged where
  fmap f (Tagged tag a) = Tagged tag (f a)

ban :: Tag -> Tagged a -> Maybe a
ban bannedTag (Tagged tag a) =
  if tag == bannedTag
     then Nothing
     else Just a

taggedPlus :: Tagged Int -> Tagged Int -> AF.Ap Tagged Int
taggedPlus a b =
  (+) <$> AF.liftAp a <*> AF.liftAp b

runBanned :: Tag -> AF.Ap Tagged a -> Maybe a
runBanned tag =
  AF.runAp (ban tag)

tags :: AF.Ap Tagged a -> Set.Set Tag
tags =
  AF.runAp_ tagSet
    where
      tagSet (Tagged tag _) = Set.singleton tag

runIter :: AF.Ap Tagged a -> a
runIter =
  AF.iterAp untag
    where
      untag (Tagged _ a) = a

runBannedHoist :: Tag -> AF.Ap Tagged a -> Maybe a
runBannedHoist bannedTag =
  AF.retractAp . AF.hoistAp (ban bannedTag)


{--
  DSL Encoding
  (completely different meaning of tagged!)
--}

-- Tagged Initial

data TaggedInitial
  = TI_StringValue String
  | TI_IntValue Int
  | TI_Concat TaggedInitial TaggedInitial
  | TI_Add TaggedInitial TaggedInitial

data TaggedInitialResult
  = TI_StringResult String
  | TI_IntResult Int
  deriving Show

evalTaggedInitial :: TaggedInitial -> Maybe TaggedInitialResult
evalTaggedInitial expr =
  case expr of
    TI_StringValue string -> pure $ TI_StringResult string
    TI_IntValue int -> pure $ TI_IntResult int
    TI_Concat a b -> do
      resultA <- evalTaggedInitial a
      resultB <- evalTaggedInitial b
      case (resultA, resultB) of
        (TI_StringResult strA, TI_StringResult strB) ->
          pure $ TI_StringResult (strA ++ strB)
        _ ->
          Nothing

    TI_Add a b -> do
      resultA <- evalTaggedInitial a
      resultB <- evalTaggedInitial b
      case (resultA, resultB) of
        (TI_IntResult intA, TI_IntResult intB) ->
          pure $ TI_IntResult (intA + intB)
        _ ->
          Nothing

data TaglessInitial a where
  TLESS_String :: String -> TaglessInitial String
  TLESS_Int :: Int -> TaglessInitial Int
  TLESS_Concat :: TaglessInitial String -> TaglessInitial String -> TaglessInitial String
  TLESS_Add :: TaglessInitial Int -> TaglessInitial Int -> TaglessInitial Int

evalTaglessInitial :: TaglessInitial a -> a
evalTaglessInitial expr =
  case expr of
    TLESS_String s -> s
    TLESS_Int i -> i
    TLESS_Concat a b -> evalTaglessInitial a ++ evalTaglessInitial b
    TLESS_Add a b -> evalTaglessInitial a + evalTaglessInitial b


class FinalStr a where
  strValue :: String -> a String
  concat' :: a String -> a String -> a String

class FinalInt a where
  intValue :: Int -> a Int
  add :: a Int -> a Int -> a Int

newtype FinalValue a = FinalValue a

instance FinalStr FinalValue where
  strValue = FinalValue
  concat' (FinalValue a) (FinalValue b) = FinalValue (a ++ b)

instance FinalInt FinalValue where
  intValue = FinalValue
  add (FinalValue a) (FinalValue b) = FinalValue (a + b)

eval :: FinalValue a -> a
eval (FinalValue a) = a

anInt :: Int
anInt =
  eval $ add (FinalValue 1) (add (FinalValue 2) (FinalValue 3))

aString :: String
aString =
  eval $ concat' (concat' (FinalValue "Hello") (FinalValue " ")) (FinalValue "World")
