module Year2020.Month02.Day29 where

{--

Semigroup

- Write out the Semigroup law

- Implement combineList
- Implement combineOrdering
- Implement combineMaybe (Using Semigroup of underlying type)
- Implement combineFunction (Using Semigroup of underlying type)

- Create a newtype around Int and implement Semigroup using addition
- Create a newtype around Int and implement Semigroup using multiplication

--}

-- Semigroup law - Associativity: x <> (y <> z) = (x <> y) <> z

combineList1 :: [a] -> [a] -> [a]
combineList1 front back =
  case front of
    [] -> back
    (a:as) -> a : combineList1 as back

combineList2 :: [a] -> [a] -> [a]
combineList2 = (++)

combineOrdering :: Ordering -> Ordering -> Ordering
combineOrdering first second =
  case first of
    EQ -> second
    _  -> first

combineMaybe :: Semigroup a => Maybe a -> Maybe a -> Maybe a
combineMaybe left right =
  case (left, right) of
    (Nothing, something) -> something
    (something, Nothing) -> something
    (Just someLeft, Just someRight) -> Just (someLeft <> someRight)

combineFunction :: Semigroup b => (a -> b) -> (a -> b) -> a -> b
combineFunction left right a =
  left a <> right a

newtype Addition =
  Addition
    { getAddition :: Int
    }

instance Semigroup Addition where
  a <> b =
    Addition (getAddition a + getAddition b)

newtype Multiplication =
  Multiplication
    { getMultiplication :: Int
    }

instance Semigroup Multiplication where
  a <> b =
    Multiplication (getMultiplication a * getMultiplication b)

{--

Monoid

- Write out the Monoid laws for mempty

- Implement memptyList
- Implement memptyOrdering
- Implement memptyMaybe
- Implement memptyFunction

- Create a newtype around Int and implement Monoid using addition
  * If you're doing this at the same time as Semigroup, use the same
    newtype you created in that rep. Otherwise, you'll need to implement
    Semigroup in order to implement Monoid

- Create a newtype around Int and implement Monoid using multiplication
  * If you're doing this at the same time as Semigroup, use the same
    newtype you created in that rep. Otherwise, you'll need to implement
    Semigroup in order to implement Monoid

--}


-- Monoid laws:
--   mempty is the identify of `<>`
--     a <> mempty = a
--     mempty <> a = a

memptyList :: [a]
memptyList = []

memptyOrdering :: Ordering
memptyOrdering = EQ

memptyMaybe :: Maybe a
memptyMaybe = Nothing

memptyFunction :: Monoid b => (a -> b)
memptyFunction _ = mempty

instance Monoid Addition where
  mempty = Addition 0

instance Monoid Multiplication where
  mempty = Multiplication 1


