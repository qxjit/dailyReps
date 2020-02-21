module Reps20200221 where

import qualified Control.Applicative.Free as AF
import qualified Control.Monad as CM
import qualified Data.List as List
import qualified Data.Ord as Ord
import qualified Data.Set as Set

{--
  Data.Ord / Sorting lists

  - define an enum for player rank and build a manual Ord instance for it
  - sort a list of ranks into ascending order
  - sort a list of ranks into descinding ordering using Ord.Down
  - define a Player record with name and rank
      - Sort a list of players alphabetically using sortBy / comparing
      - Sort a list of players by rank using sortOn
  - Build a compare function for players that compares by name and then rank
      - Hint: use Ord.comparing to compare the fields
      - build one that is fully explicit
      - Then build one that uses the Ordering Semigroup
      - Then build one that uses the Ordering Semigroup and the (->) Semigroup

--}

data Rank
  = Gold
  | Silver
  | Bronze
  | Coal
  deriving (Eq, Show)

instance Ord Rank where
  compare left right =
    case (left, right) of
      (Gold   , Gold  ) -> EQ
      (Silver , Silver) -> EQ
      (Bronze , Bronze) -> EQ
      (Coal   , Coal  ) -> EQ
      (Gold   , _     ) -> GT
      (Silver , _     ) -> GT
      (Bronze , _     ) -> GT
      (Coal   , _     ) -> LT

ranks :: [Rank]
ranks =
  [ Bronze, Coal, Gold, Silver, Bronze, Gold, Silver, Coal ]

ascendingRanks :: [Rank]
ascendingRanks =
  List.sort ranks

descendingRanks :: [Rank]
descendingRanks =
  List.sortOn Ord.Down ranks

data Player =
  Player
    { playerName :: String
    , playerRank :: Rank
    }

players :: [Player]
players =
  [ Player "Cheryl" Gold
  , Player "Santa" Coal
  , Player "Sasha" Silver
  , Player "Anthony" Bronze
  ]

alphabeticalPlayers :: [Player]
alphabeticalPlayers =
  List.sortOn playerName players

rankedPlayers :: [Player]
rankedPlayers =
  List.sortBy (Ord.comparing playerRank) players

comparePlayers1 :: Player -> Player -> Ordering
comparePlayers1 left right =
  case Ord.comparing playerRank left right of
    LT -> LT
    GT -> GT
    EQ -> Ord.comparing playerName left right

comparePlayers2 :: Player -> Player -> Ordering
comparePlayers2 left right =
  Ord.comparing playerRank left right
  <> Ord.comparing playerName left right

comparePlayers3 :: Player -> Player -> Ordering
comparePlayers3 =
  Ord.comparing playerRank <> Ord.comparing playerName

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
  deriving (Show, Eq)

instance Functor Tagged where
  fmap f (Tagged tag a) =
    Tagged tag (f a)

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

runPlain :: AF.Ap Tagged a -> a
runPlain =
  AF.iterAp extractValue
    where
      extractValue (Tagged _ a) =
        a

runBanned2 :: Tag -> AF.Ap Tagged a ->  Maybe a
runBanned2 bannedTag =
  AF.retractAp . AF.hoistAp (ban bannedTag)
