{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Prim where

import Sample.Dominion.Base

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Zone
import TableGameCombinator.Zone.State

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import qualified Data.MultiSet as MS

-- Action
play :: DomDevice Dom => Card -> Dom Card
play card = do
   tell $ Play card
   moveZone hand (select card) playField bottom
   cardEffect card
   return card

playAction :: DomDevice Dom => Card -> Dom Card
playAction card = plusAction (-1) *> play card

buy :: DomDevice Dom => Card -> Dom Card
buy card = do
   tell $ Buy card
   coinCount' <- get coinCount
   plusBuy (-1)
   plusCoin (-cardCost card)
   gainCard card

canBuy :: DomDevice Dom => Card -> Dom Bool
canBuy card = (cardCost card <=) <$> get coinCount

gainCard :: DomDevice Dom => Card -> Dom Card
gainCard card = do
   moveZone supply (select card) discardPile iAny
   return card

gainBy :: DomDevice Dom => (Card -> Bool) -> Dom (Maybe Card)
gainBy f = chooseBy cardName gainCard =<< filter f <$> gets supply MS.distinctElems

gainUpTo :: DomDevice Dom => Int -> Dom (Maybe Card)
gainUpTo coin = gainBy ((<=coin) . cardCost)

draw :: DomDevice Dom => Dom (Maybe Card)
draw = do
   mcard <- moveZone deck top hand iAny
   case mcard of
      Just card -> do
         tell $ Draw card
         return $ Just card
      Nothing   -> do
         moveZoneAll discardPile oAny deck top
         shuffleDeck
         mcard' <- moveZone deck top hand iAny
         case mcard' of
            Just card -> do
               tell $ Draw card
               return $ Just card
            Nothing -> return Nothing

trashCardFromHand :: DomDevice Dom => Card -> Dom Card
trashCardFromHand card = do
   tell $ Trash card
   moveZone hand (select card) trashPile iAny
   return card

trashFromHand :: DomDevice Dom => Dom (Maybe Card)
trashFromHand = trashFromHandBy $ \_ -> True

trashFromHandBy :: DomDevice Dom => (Card -> Bool) -> Dom (Maybe Card)
trashFromHandBy f = chooseBy cardName trashCardFromHand =<< filter f <$> gets hand MS.distinctElems

shuffleDeck :: DomDevice Dom => Dom ()
shuffleDeck = do
   (d, zero) <- gets deck (toList top)
   d' <- shuffleM d
   set deck $ fromList top d' zero
   tell $ "You shuffle your deck.\n"

plusCoin :: Int -> Dom ()
plusCoin n = modify coinCount (+n)

plusAction :: Int -> Dom ()
plusAction n = modify actionCount (+n)

plusBuy :: Int -> Dom ()
plusBuy n = modify buyCount (+n)

plusCard :: DomDevice Dom => Int -> Dom ()
plusCard n = replicateM_ n draw

-- IO (only) Action
tellInfo :: DomDevice Dom => Dom ()
tellInfo = tell =<< getAll

-- vim: set expandtab:
