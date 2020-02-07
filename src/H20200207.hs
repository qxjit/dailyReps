module H20200207 where

import qualified Control.Applicative.Free as AF

-- Use Control.Applicative.Free to add Applicative to a Functor

data Tag = Red | Black deriving (Show, Eq)
data Tagged a = Tagged Tag a

instance Functor Tagged where
  fmap f (Tagged tag a) = Tagged tag (f a)

red :: a -> Tagged a
red = Tagged Red

black :: a -> Tagged a
black = Tagged Black

-- use liftAp

taggedPlus :: Tagged Int -> Tagged Int -> AF.Ap Tagged Int
taggedPlus a b =
  (+) <$> AF.liftAp a <*> AF.liftAp b

-- use runAp

ban :: Tag -> Tagged a -> Maybe a
ban bannedTag (Tagged tag a) =
  if tag == bannedTag
     then Nothing
     else Just a

runBanned :: Tag -> AF.Ap Tagged Int -> Maybe Int
runBanned bannedTag =
  AF.runAp (ban bannedTag)

-- use runAp_

tags :: AF.Ap Tagged Int -> [Tag]
tags =
  AF.runAp_ getTags
    where
      getTags (Tagged tag _) = [tag]

-- use iterAp

iterTag :: AF.Ap Tagged a -> a
iterTag =
  AF.iterAp untag
    where
      untag (Tagged _ a) = a

-- use hoistAp
-- use retractAp

runBannedRetract :: Tag -> AF.Ap Tagged Int -> Maybe Int
runBannedRetract bannedTag =
  AF.retractAp . AF.hoistAp (ban bannedTag)

