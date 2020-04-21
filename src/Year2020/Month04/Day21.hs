module Year2020.Month04.Day21 where

data Binary
  = BinZero
  | BinOne
  deriving (Eq, Show)

addBinary :: Binary -> Binary -> (Binary, Binary)
addBinary left right =
  case (left, right) of
    (BinZero, BinZero) -> (BinZero, BinZero)
    (BinZero, BinOne ) -> (BinZero, BinOne)
    (BinOne , BinZero) -> (BinZero, BinOne)
    (BinOne , BinOne)  -> (BinOne,  BinZero)

data Ternary
  = TerZero
  | TerOne
  | TerTwo
  deriving (Eq, Show)

addTernary :: Ternary -> Ternary -> (Ternary, Ternary)
addTernary left right =
  case (left, right) of
    (TerZero , anything) -> (TerZero, anything)
    (anything, TerZero ) -> (TerZero, anything)
    (TerOne  , TerOne  ) -> (TerZero, TerTwo)
    (TerOne  , TerTwo  ) -> (TerOne,  TerZero)
    (TerTwo  , TerOne  ) -> (TerOne,  TerZero)
    (TerTwo  , TerTwo  ) -> (TerOne,  TerOne)

{-
   Laws:

   Identity:
     add x zero == x
     add zero x == x

   Associative:
     add x (add y z) == add (add x y) add z

   Commutative:
     add x y == add y x

-}
class Digit digit where
  addDigit :: digit -> digit -> (digit, digit)
  zero :: digit

instance Digit Binary where
  addDigit = addBinary
  zero = BinZero

instance Digit Ternary where
  addDigit = addTernary
  zero = TerZero

add :: (Digit digit, Eq digit) => [digit] -> [digit] -> [digit]
add left right =
  reverse $ go zero (reverse left) (reverse right)
    where
      go carry as bs =
        case (as, bs) of
          ([], []) ->
            if carry == zero
            then []
            else [carry]

          ([], b:restB) ->
            let
              (newCarry, newDigit) = addDigit carry b
            in
              newDigit : go newCarry [] restB

          (a:restA, []) ->
            let
              (newCarry, newDigit) = addDigit carry a
            in
              newDigit : go newCarry restA []

          (a:restA, b:restB) ->
            let
              (abCarry,    abDigit)  = addDigit a b
              (carryCarry, newDigit) = addDigit carry abDigit
              (_,          newCarry) = addDigit carryCarry abCarry
            in
              newDigit : go newCarry restA restB

