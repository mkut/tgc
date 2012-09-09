{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sample.Dominion.Base where
   
import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Zone
import TableGameCombinator.Tag

import System.IO
import Control.Applicative
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as S
import qualified Data.List as List
import Data.Label (mkLabelsMono, Lens)
import qualified Data.Label as L
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Foldable as Fold (Foldable, toList)

-- Game Monad
type Dom = StateT DominionState IO

-- Card
data CardType = Treasure
              | Victory
              | Curse
              | Action
              deriving (Show, Eq)
data Card = Card
   { cardName   :: String
   , cardType   :: CardType
   , cardCost   :: Int
   , cardVP     :: Int
   , cardEffect :: Int -> Dom ()
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
              deriving (Show, Eq)

-- GameState
data DominionState = DS
   { _phase       :: DomPhase
   , _deck        :: [Card]
   , _hand        :: MultiSet Card
   , _playField   :: [TCard]
   , _discardPile :: MultiSet Card
   , _trashPile   :: MultiSet Card
   , _supply      :: MultiSet Card
   , _actionCount :: Int
   , _coinCount   :: Int
   , _buyCount    :: Int
   }
mkLabelsMono [''DominionState]

initialState :: DominionState
initialState = DS
   { _phase       = ActionPhase
   , _deck        = []
   , _hand        = MS.empty
   , _playField   = []
   , _discardPile = MS.empty
   , _trashPile   = MS.empty
   , _supply      = MS.empty
   , _actionCount = 0
   , _coinCount   = 0
   , _buyCount    = 0
   }

-- Log
data Log = Draw Card
         | Trash Card
         | Play Card
         | Buy Card
         | Gain Card
         | Shuffle

-- I/O Device
class ( IDevice m String
      , IDevice m YesNoInput
      , ODevice m String
      , ODevice m [String]
      , ODevice m DominionState
      , ODevice m Log
      )
      => DomDevice m where

-- Zone Port
type DomIPort z a = ZoneIPort (Lens (->)) DominionState (z a) a
type DomDPort z a = ZoneDPort (Lens (->)) DominionState (z a) a

msDeletePort :: Ord a => a -> DeletePort (MultiSet a) a
msDeletePort x = deletePort f g
   where
      f z = if MS.member x z then Just x else Nothing
      g = MS.delete x

msDeletePortAny :: Ord a => DeletePort (MultiSet a) a
msDeletePortAny z = if MS.null z
   then Nothing
   else msDeletePort (head $ MS.distinctElems z) z

toDeckTop :: DomIPort [] Card
toDeckTop = (deck, (:))
fromDeckTop :: DomDPort [] Card
fromDeckTop = (deck, listDeletePort)

toHand :: DomIPort MultiSet Card
toHand = (hand, MS.insert)
fromHand :: Card -> DomDPort MultiSet Card
fromHand c = (hand, msDeletePort c)
fromHandAny :: DomDPort MultiSet Card
fromHandAny = (hand, msDeletePortAny)

toPlay :: DomIPort [] TCard
toPlay = (playField, (:))
fromPlay :: DomDPort [] TCard
fromPlay = (playField, listDeletePort)
fromPlayTagged :: Tag -> DomDPort [] TCard
fromPlayTagged t = (playField, dp)
   where
      dp z = case List.find (tagged t) z of
         Nothing -> Nothing
         Just x  -> Just (x, List.delete x z)

toDiscard :: DomIPort MultiSet Card
toDiscard = (discardPile, MS.insert)
fromDiscardAny :: DomDPort MultiSet Card
fromDiscardAny = (discardPile, msDeletePortAny)

toTrash :: DomIPort MultiSet Card
toTrash = (trashPile, MS.insert)

fromSupply :: Card -> DomDPort MultiSet Card
fromSupply c = (supply, msDeletePort c)

-- vim: set expandtab:
