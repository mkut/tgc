{-# LANGUAGE TemplateHaskell #-}
module Sample.Dominion.Base where
   
import TableGameCombinator.Core

import Data.Label (mkLabel)

-- Types
type Dom  = Process DominionState

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

-- Phase
data DomPhase = ActionPhase
              | MoneyPhase
              | BuyPhase
              | EndPhase
              deriving (Show, Eq)

-- GameState
data DominionState = DS
   { _phase       :: DomPhase
   , _deck        :: [Card]
   , _hand        :: [Card]
   , _playField   :: [Card]
   , _discardPile :: [Card]
   , _trashPile   :: [Card]
   , _supply      :: [(Card, Int)]
   , _actionCount :: Int
   , _coinCount   :: Int
   , _buyCount    :: Int
   }
mkLabel ''DominionState

initialState :: DominionState
initialState = DS
   { _phase       = ActionPhase
   , _deck        = []
   , _hand        = []
   , _playField   = []
   , _discardPile = []
   , _trashPile   = []
   , _supply      = []
   , _actionCount = 0
   , _coinCount   = 0
   , _buyCount    = 0
   }

-- vim: set expandtab:
