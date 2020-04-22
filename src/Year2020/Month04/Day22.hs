module Year2020.Month04.Day22 where

data BinaryDigit
  = BinZero
  | BinOne
  deriving (Show, Eq)

addBinaryBigit :: BinaryDigit -> BinaryDigit -> (BinaryDigit, BinaryDigit)
addBinaryBigit left right =
  case (left, right) of
    (BinZero, BinZero) -> (BinZero, BinZero)
    (BinZero, BinOne ) -> (BinZero, BinOne )
    (BinOne,  BinZero) -> (BinZero, BinOne )
    (BinOne,  BinOne ) -> (BinOne,  BinZero)

formatBinaryDigit :: BinaryDigit -> Char
formatBinaryDigit digit =
  case digit of
    BinZero -> '0'
    BinOne  -> '1'


data TernaryDigit
  = TerZero
  | TerOne
  | TerTwo
  deriving (Show, Eq)

formatTernaryDigit :: TernaryDigit -> Char
formatTernaryDigit digit =
  case digit of
    TerZero -> '0'
    TerOne  -> '1'
    TerTwo  -> '2'

addTernaryDigit :: TernaryDigit -> TernaryDigit -> (TernaryDigit, TernaryDigit)
addTernaryDigit left right =
  case (left, right) of
    (TerZero ,  anything) -> (TerZero, anything)
    (anything,  TerZero ) -> (TerZero, anything)
    (TerOne  ,  TerOne  ) -> (TerZero, TerTwo  )
    (TerOne  ,  TerTwo  ) -> (TerOne , TerZero )
    (TerTwo  ,  TerOne  ) -> (TerOne , TerZero )
    (TerTwo  ,  TerTwo  ) -> (TerOne , TerOne  )

{-
  Digit Laws:

  Identity:
    addDigit zero x == (zero, x)

  Commutative:
    addDigit x y == addDigit y x

-}

class Eq digit => Digit digit where
  zero :: digit
  addDigit :: digit -> digit -> (digit, digit)
  formatDigit :: digit -> Char

instance Digit BinaryDigit where
  zero =
    BinZero

  addDigit =
    addBinaryBigit

  formatDigit =
    formatBinaryDigit

instance Digit TernaryDigit where
  zero =
    TerZero

  addDigit =
    addTernaryDigit

  formatDigit =
    formatTernaryDigit

formatNumber :: Digit digit => [digit] -> String
formatNumber =
  reverse . map formatDigit

addSingleton :: Digit digit => digit -> [digit] -> [digit]
addSingleton digit num =
  case num of
    [] ->
      if digit == zero
      then []
      else [digit]

    leastSignificant : rest ->
      let
        (carry, newDigit) = addDigit digit leastSignificant
      in
        newDigit : addSingleton carry rest

add :: Digit digit => [digit] -> [digit] -> [digit]
add =
  go zero
    where
      go carry as bs =
        case (as, bs) of
          ([], _) ->
            addSingleton carry bs

          (_, []) ->
            addSingleton carry as

          (digitA : restA, digitB : restB) ->
            let
              (abCarry   , abDigit)  = addDigit digitA digitB
              (carryCarry, newDigit) = addDigit carry abDigit
              (_         , newCarry) = addDigit abCarry carryCarry
            in
              newDigit : go newCarry restA restB

