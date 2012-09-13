{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sample.Dominion.Base where
   
import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Zone
import TableGameCombinator.Tag
import TableGameCombinator.Player (PlayerLens)
import qualified TableGameCombinator.Player as P

import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as S
import Control.Monad.Trans.Class
import qualified Data.List as List
import Data.Array
import Data.Label (mkLabelsMono, Lens, (:->))
import qualified Data.Label as L
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Foldable as Fold (Foldable, toList)

-- Game Monad
type Dom = StateT DominionState IO
type MyDom = ReaderT Player Dom

-- Card
data CardType = Treasure
              | Victory
              | Curse
              | Action
              deriving (Show, Eq)
data Card = Card
   { cardName   :: String
   , cardType   :: CardType
   , cardCost   :: Dom Int
   , cardVP     :: MultiSet Card -> Int
   , cardEffect :: Int -> MyDom ()
   }
instance Eq Card where
   x == y = cardName x == cardName y
instance Ord Card where
   compare x y = compare (cardName x) (cardName y)

type TCard = Tagged Card

-- Phase
data DomPhase = ActionPhase
              | MoneyPhase
              | BuyPhase
              | CleanUpPhase
              deriving (Show, Eq, Enum)

-- Player
data Player = Player1
            | Player2
            deriving (Eq, Ord, Enum, Show, Ix)

players :: [Player]
players = [Player1 ..]

opponents :: MyDom [Player]
opponents = flip filter players . (/=) <$> R.ask

eachPlayersDo :: [Player] -> MyDom a -> Dom ()
eachPlayersDo pls proc = forM_ pls $ runReaderT proc

eachOpponentsDo :: MyDom a -> MyDom ()
eachOpponentsDo proc = do
   ops <- opponents
   lift $ eachPlayersDo ops proc
-- GameState
data DominionState = DS
   { _activePlayer :: Player
   , _phase        :: DomPhase
   , _deck         :: Array Player [Card]
   , _hand         :: Array Player (MultiSet Card)
   , _playField    :: Array Player [TCard]
   , _discardPile  :: Array Player (MultiSet Card)
   , _trashPile    :: MultiSet Card
   , _supply       :: MultiSet Card
   , _aside        :: MultiSet Card
   , _actionCount  :: Array Player Int
   , _coinCount    :: Array Player Int
   , _buyCount     :: Array Player Int
   }
mkLabelsMono [''DominionState]

initialState :: DominionState
initialState = DS
   { _activePlayer = Player1
   , _phase        = ActionPhase
   , _deck         = array (Player1, Player2) []
   , _hand         = array (Player1, Player2) []
   , _playField    = array (Player1, Player2) []
   , _discardPile  = array (Player1, Player2) []
   , _trashPile    = MS.empty
   , _supply       = MS.empty
   , _aside        = MS.empty
   , _actionCount  = array (Player1, Player2) []
   , _coinCount    = array (Player1, Player2) []
   , _buyCount     = array (Player1, Player2) []
   }

-- PlayerLens
phase' :: PlayerLens Player DominionState DomPhase
phase' = P.fromLens phase

deck' :: PlayerLens Player DominionState [Card]
deck' = P.fromArrayLens deck

hand' :: PlayerLens Player DominionState (MultiSet Card)
hand' = P.fromArrayLens hand

playField' :: PlayerLens Player DominionState [TCard]
playField' = P.fromArrayLens playField

discardPile' :: PlayerLens Player DominionState (MultiSet Card)
discardPile' = P.fromArrayLens discardPile

trashPile' :: PlayerLens Player DominionState (MultiSet Card)
trashPile' = P.fromLens trashPile

supply' :: PlayerLens Player DominionState (MultiSet Card)
supply' = P.fromLens supply

aside' :: PlayerLens Player DominionState (MultiSet Card)
aside' = P.fromLens aside

actionCount' :: PlayerLens Player DominionState Int
actionCount' = P.fromArrayLens actionCount

coinCount' :: PlayerLens Player DominionState Int
coinCount' = P.fromArrayLens coinCount

buyCount' :: PlayerLens Player DominionState Int
buyCount' = P.fromArrayLens buyCount

-- Log
data Log = Draw Card
         | Discard Card
         | Trash Card
         | Play Card
         | Reveal Card
         | Buy Card
         | Gain Card
         | Shuffle

-- I/O Device
class ( IDevice m String
      , IDevice m YesNoInput
      , IDevice m [String]
      , ODevice m String
      , ODevice m [String]
      , ODevice m DominionState
      , ODevice m Log
      , ODevice m (DomPhase, Player)
      , ODevice m Int -- for score
      )
      => DomDevice m where

tellAll :: (RecordMonadState DominionState m (Lens (->)), ODevice (ReaderT Player m) o) => o -> m ()
tellAll x = do
   forM_ [Player1 ..] $ runReaderT (tell x)

instance ODevice MyDom o => ODevice Dom o where
   tell = tellAll

-- Zone Port
type DomIPort z a = ZoneIPort (Lens (->)) DominionState (z a) a
type DomDPort z a = ZoneDPort (Lens (->)) DominionState (z a) a
type MyDomIPort z a = ZoneIPort (PlayerLens Player) DominionState (z a) a
type MyDomDPort z a = ZoneDPort (PlayerLens Player) DominionState (z a) a

msDeletePort :: Ord a => a -> DeletePort (MultiSet a) a
msDeletePort x = deletePort f g
   where
      f z = if MS.member x z then Just x else Nothing
      g = MS.delete x

msDeletePortAny :: Ord a => DeletePort (MultiSet a) a
msDeletePortAny z = if MS.null z
   then Nothing
   else msDeletePort (head $ MS.distinctElems z) z

msDeletePortBy :: Ord a => (a -> Bool) -> DeletePort (MultiSet a) a
msDeletePortBy f z = case List.find f $ MS.distinctElems z of
   Nothing -> Nothing
   Just x -> msDeletePort x z

toDeckTop :: MyDomIPort [] Card
toDeckTop = (deck', (:))
fromDeckTop :: MyDomDPort [] Card
fromDeckTop = (deck', listDeletePort)

toHand :: MyDomIPort MultiSet Card
toHand = (hand', MS.insert)
fromHand :: Card -> MyDomDPort MultiSet Card
fromHand c = (hand', msDeletePort c)
fromHandAny :: MyDomDPort MultiSet Card
fromHandAny = (hand', msDeletePortAny)

toPlay :: MyDomIPort [] TCard
toPlay = (playField', (:))
fromPlay :: MyDomDPort [] TCard
fromPlay = (playField', listDeletePort)
fromPlayTagged :: Tag -> MyDomDPort [] TCard
fromPlayTagged t = (playField', dp)
   where
      dp z = case List.find (tagged t) z of
         Nothing -> Nothing
         Just x  -> Just (x, List.delete x z)

toDiscard :: MyDomIPort MultiSet Card
toDiscard = (discardPile', MS.insert)
fromDiscardAny :: MyDomDPort MultiSet Card
fromDiscardAny = (discardPile', msDeletePortAny)

toTrash :: MyDomIPort MultiSet Card
toTrash = (trashPile', MS.insert)

fromSupply :: Card -> MyDomDPort MultiSet Card
fromSupply c = (supply', msDeletePort c)

toAside :: MyDomIPort MultiSet Card
toAside = (aside', MS.insert)
fromAsideAny :: MyDomDPort MultiSet Card
fromAsideAny = (aside', msDeletePortAny)
fromAsideBy :: (Card -> Bool) -> MyDomDPort MultiSet Card
fromAsideBy f = (aside', msDeletePortBy f)

-- vim: set expandtab:
