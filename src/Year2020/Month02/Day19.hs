module Year2020.Month02.Day19 where

import qualified Data.List as List
import qualified Data.Ord as Ord

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
  = Worst
  | Ok
  | Best
  deriving (Show, Eq)

instance Ord Rank where
  compare left right =
    case (left, right) of
      (Worst, Worst) -> EQ
      (Best , Best ) -> EQ
      (Ok   , Ok   ) -> EQ

      (Worst, _)     -> LT
      (_,     Worst) -> GT
      (Best , _    ) -> GT
      (_    , Best ) -> LT

ranks :: [Rank]
ranks =
  [Ok, Ok, Worst, Best, Worst]

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
    } deriving (Show, Eq)

players :: [Player]
players =
  [ Player "Bob" Ok
  , Player "Sally" Best
  , Player "Patricia" Ok
  , Player "Dan" Worst
  ]

alphabeticalPlayers :: [Player]
alphabeticalPlayers =
  List.sortBy (Ord.comparing playerName) players

rankedPlayers :: [Player]
rankedPlayers =
  List.sortOn playerRank players

comparePlayers1 :: Player -> Player -> Ordering
comparePlayers1 left right =
  case Ord.comparing playerName left right of
    LT -> LT
    GT -> GT
    EQ -> Ord.comparing playerRank left right

comparePlayers2 :: Player -> Player -> Ordering
comparePlayers2 left right =
     Ord.comparing playerName left right
  <> Ord.comparing playerRank left right

comparePlayers3 :: Player -> Player -> Ordering
comparePlayers3 =
  Ord.comparing playerName <> Ord.comparing playerRank

