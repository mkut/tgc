{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Base where
   
import TableGameCombinator.Core

import System.IO
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Label (mkLabel, (:->))
import qualified Data.Label as L
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

-- Game Monad
type Dom = S.StateT DominionState IO

-- I/O Device
class ( IDevice m String
      , ODevice m String
      , ODevice m [String]
      )
      => DomDevice m where

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
mkLabel ''DominionState

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

-- State Operations
get :: (DominionState :-> a) -> Dom a
get lens = gets lens id

gets :: (DominionState :-> a) -> (a -> b) -> Dom b
gets lens f = S.gets (f . L.get lens)

modify :: (DominionState :-> a) -> (a -> a) -> Dom ()
modify lens f = S.modify (L.modify lens f)

set :: (DominionState :-> a) -> a -> Dom ()
set lens x = S.modify (L.set lens x)

-- vim: set expandtab:
