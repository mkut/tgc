{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
module Sample.Dominion
   ( main
   ) where

import Sample.Dominion.Base
import Sample.Dominion.Phase
import Sample.Dominion.Prim
import Sample.Dominion.CardData

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Phase
import TableGameCombinator.Tag
import TableGameCombinator.Player

import System.Random.Shuffle
import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Data.Array
import qualified Data.Label as L
import qualified Data.MultiSet as MS

main :: DomDevice MyDom => Dom ()
main = do
   initialize
   phaseController (ActionPhase, Player1)

initialize :: DomDevice MyDom => Dom ()
initialize = do
   set deck        $ listArray (Player1, Player2) $ repeat []
   set hand        $ listArray (Player1, Player2) $ repeat MS.empty
   set playField   $ listArray (Player1, Player2) $ repeat []
   set discardPile $ listArray (Player1, Player2) $ repeat MS.empty
   set actionCount $ listArray (Player1, Player2) $ repeat 0
   set coinCount   $ listArray (Player1, Player2) $ repeat 0
   set buyCount    $ listArray (Player1, Player2) $ repeat 0
   custom <- take 10 <$> shuffleM customSupply
   set supply $ MS.fromOccurList $ defaultSupply ++ map (,10) custom
   forM_ [Player1 ..] $ runReaderT $ do
      set discardPile' initialDeck
      plusCard 5
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
         , thief
         , adventurer
         , moat
         , witch
         , spy
         , milita
         , village
         , bureaucrat
         , chapel
         ]

-- vim: set expandtab:
