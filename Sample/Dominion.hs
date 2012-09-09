{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
module Sample.Dominion
   ( main
   ) where

import Sample.Dominion.Base
import Sample.Dominion.Phase
import Sample.Dominion.CardData

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Phase
import TableGameCombinator.Tag

import System.Random.Shuffle
import Control.Applicative
import qualified Data.Label as L
import qualified Data.MultiSet as MS

main :: DomDevice Dom => Dom ()
main = do
   initialize
   phaseController CleanUpPhase

initialize :: DomDevice Dom => Dom ()
initialize = do
   custom <- take 10 <$> shuffleM customSupply
   set supply $ MS.fromOccurList $ defaultSupply ++ map (,10) custom
   set discardPile initialDeck
   where
      initialDeck = MS.fromOccurList
         [ (copper, 7)
         , (estate, 3)
         ]
      defaultSupply =
         [ (copper, 53)
         , (silver, 40)
         , (gold, 30)
         , (estate, 4)
         , (duchy, 4)
         , (province, 4)
         ]
      customSupply =
         [ market
         , remodel
         , smithy
         , moneylender
         , woodcutter
         , councilRoom
         , throneRoom
         , laboratory
         , mine
         , workshop
         , chancellor
         , feast
         , festival
         , library
         , cellor
         , gardens
         ]

-- vim: set expandtab:
