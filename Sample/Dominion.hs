{-# LANGUAGE FlexibleContexts #-}
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
import qualified Data.Label as L
import qualified Data.MultiSet as MS

main :: DomDevice Dom => Dom ()
main = do
   initialize
   phaseController CleanUpPhase

initialize :: DomDevice Dom => Dom ()
initialize = do
   set supply initialSupply
   set discardPile initialDeck
   where
      initialDeck = MS.fromOccurList
         [ (copper, 7)
         , (estate, 3)
         ]
      initialSupply = MS.fromOccurList
         [ (copper, 53)
         , (silver, 40)
         , (gold, 30)
         , (estate, 4)
         , (duchy, 4)
         , (province, 4)
         , (curse, 0)
         , (market, 10)
         , (remodel, 10)
         , (smithy, 10)
         , (moneylender, 10)
         , (woodcutter, 10)
         , (councilRoom, 10)
         , (throneRoom, 10)
         , (laboratory, 10)
         , (mine, 10)
         , (workshop, 10)
         , (chancellor, 10)
         , (feast, 10)
         ]

-- vim: set expandtab:
