module Sample.Dominion
   ( main
   , initialState
   ) where

import Sample.Dominion.Base
import Sample.Dominion.Phase
import Sample.Dominion.CardData

import TableGameCombinator.Core
import TableGameCombinator.Phase
import qualified Data.Label as L

main :: Dom ()
main = do
   initialize
   phaseController EndPhase

initialize :: Dom ()
initialize = do
   modify (L.set supply initialSupply)
   modify (L.set discardPile initialDeck)
   where
      initialDeck = replicate 7 copper ++ replicate 3 estate
      initialSupply =
         [ (copper, 53)
         , (silver, 40)
         , (gold, 30)
         , (estate, 4)
         , (duchy, 4)
         , (province, 4)
         , (curse, 0)
         ]

-- vim: set expandtab:
