{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Base where
   
import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Zone

import System.IO
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as S
import Data.Label (mkLabelsMono)
import qualified Data.Label as L
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
   , cardEffect :: Dom ()
   }
instance Eq Card where
   x == y = cardName x == cardName y
instance Ord Card where
   compare x y = compare (cardName x) (cardName y)

-- Phase
data DomPhase = ActionPhase
              | MoneyPhase
              | BuyPhase
              | CleanUpPhase
              deriving (Show, Eq)

-- GameState
data DominionState = DS
   { _phase       :: DomPhase
   , _deck        :: Seq Card
   , _hand        :: MultiSet Card
   , _playField   :: Seq Card
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
   , _deck        = Seq.empty
   , _hand        = MS.empty
   , _playField   = Seq.empty
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
         | Shuffle

-- I/O Device
class ( IDevice m String
      , ODevice m String
      , ODevice m [String]
      , ODevice m DominionState
      , ODevice m Log
      )
      => DomDevice m where

-- Zone Port
top :: Top Card
top = Top

bottom :: Bottom Card
bottom = Bottom

select :: Card -> Select Card
select c = Select c

iAny :: IAny Card
iAny = IAny

oAny :: OAny Card
oAny = OAny

-- vim: set expandtab:
