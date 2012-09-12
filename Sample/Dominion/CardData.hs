{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.CardData where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import TableGameCombinator.Core
import TableGameCombinator.Choice
import TableGameCombinator.State
import TableGameCombinator.Tag
import TableGameCombinator.Zone

import Control.Monad
import Control.Applicative
import qualified Data.MultiSet as MS

copper :: DomDevice Dom => Card
copper = Card
   "Copper"
   Treasure
   (cost 0)
   (point 0)
   (wt $ plusCoin 1)
silver :: DomDevice Dom => Card
silver = Card
   "Silver"
   Treasure
   (cost 3)
   (point 0)
   (wt $ plusCoin 2)
gold :: DomDevice Dom => Card
gold  = Card
   "Gold"
   Treasure
   (cost 6)
   (point 0)
   (wt $ plusCoin 3)
estate :: DomDevice Dom => Card
estate = Card
   "Estate"
   Victory
   (cost 2)
   (point 1)
   noAction
duchy :: DomDevice Dom => Card
duchy = Card
   "Duchy"
   Victory
   (cost 5)
   (point 3)
   noAction
province :: DomDevice Dom => Card
province = Card
   "Province"
   Victory
   (cost 8)
   (point 6)
   noAction
curse :: DomDevice Dom => Card
curse = Card
   "Curse"
   Curse
   (cost 0)
   (point (-1))
   noAction
market :: DomDevice Dom => Card
market = Card
   "Market"
   Action
   (cost 5)
   (point 0)
   (wt $ plusCard 1 *> plusAction 1 *> plusBuy 1 *> plusCoin 1)
remodel :: DomDevice Dom => Card
remodel = Card
   "Remodel"
   Action
   (cost 4)
   (point 0)
   (wt $ gain `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< handOps
      gain card = chooseCard gainCard =<< filterM (costUpToDiff card 2) =<< supplyOps
smithy :: DomDevice Dom => Card
smithy  = Card
   "Smithy"
   Action
   (cost 4)
   (point 0)
   (wt $ plusCard 3)
moneylender :: DomDevice Dom => Card
moneylender = Card
   "Moneylender"
   Action
   (cost 4)
   (point 0)
   (wt $ (\_ -> plusCoin 3) `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< filter (==copper) <$> handOps
woodcutter :: DomDevice Dom => Card
woodcutter = Card
   "Woodcutter"
   Action
   (cost 3)
   (point 0)
   (wt $ plusBuy 1 *> plusCoin 2)
councilRoom :: DomDevice Dom => Card -- incomplete
councilRoom = Card
   "CouncilRoom"
   Action
   (cost 5)
   (point 0)
   (wt $ plusCard 4 *> plusBuy 1)
throneRoom :: DomDevice Dom => Card
throneRoom = Card
   "ThroneRoom"
   Action
   (cost 4)
   (point 0)
   (\t -> void $ chooseBy cardName (playCardN 2 (t+1)) =<< filter (withCardType Action) <$> handOps)
laboratory :: DomDevice Dom => Card
laboratory = Card
   "Laboratory"
   Action
   (cost 5)
   (point 0)
   (wt $ plusCard 2 *> plusAction 1)
mine :: DomDevice Dom => Card
mine = Card
   "Mine"
   Action
   (cost 5)
   (point 0)
   (wt $ gain `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< typeFilter =<< handOps
      gain card = chooseCard gainCardToHand =<< typeFilter =<< costFilter =<< supplyOps
         where
            costFilter = filterM (costUpToDiff card 3)
      typeFilter = filterM (return . withCardType Treasure)
workshop :: DomDevice Dom => Card
workshop = Card
   "Workshop"
   Action
   (cost 3)
   (point 0)
   (wt $ chooseCard gainCard =<< filterM (costUpTo 4) =<< supplyOps)
chancellor :: DomDevice Dom => Card
chancellor = Card
   "Chancellor"
   Action
   (cost 3)
   (point 0)
   (wt $ plusCoin 2 *> may msg (moveZone fromDeckTop toDiscard))
   where
      msg = "Put your deck into your discard pile? [yes/no]" 
feast :: DomDevice Dom => Card
feast = Card
   "Feast"
   Action
   (cost 4)
   (point 0)
   $ \t -> void $ trashTagged t *> gain
   where
      gain = chooseCard gainCard =<< filterM (costUpTo 5) =<< supplyOps
festival :: DomDevice Dom => Card
festival = Card
   "Festival"
   Action
   (cost 5)
   (point 0)
   (wt $ plusAction 2 *> plusBuy 1 *> plusCoin 2)
library :: DomDevice Dom => Card
library = Card
   "Library"
   Action
   (cost 5)
   (point 0)
   (wt $ doWhile drawReveal *> moveZone fromAsideAny toDiscard)
   where
      drawReveal = do
         n <- gets hand MS.size
         if (n < 7)
            then do
               mcard <- draw
               case mcard of
                  Nothing   -> return Nothing
                  Just card -> if withCardType Action card
                     then do
                        may msg $ movePort (fromHand card) toAside
                        return mcard
                     else return mcard
            else return Nothing
      msg = "Put aside this card? [yes/no]"
cellor :: DomDevice Dom => Card
cellor = Card
   "Cellor"
   Action
   (cost 2)
   (point 0)
   (wt $ plusAction 1 *> (plusCard =<< length <$> select))
   where
      select = chooseSomeBy cardName (keep discard) =<< gets hand MS.elems
gardens :: DomDevice Dom => Card
gardens = Card
   "Gardens"
   Victory
   (cost 4)
   ((`div`10) . MS.size)
   noAction
thief :: DomDevice Dom => Card
thief = Card
   "Thief"
   Action
   (cost 4)
   (point 0)
   noAction
adventurer :: DomDevice Dom => Card
adventurer = Card
   "Adventurer"
   Action
   (cost 6)
   (point 0)
   (wt $ doUntil reveal *> doUntil reveal *> toHandDiscard)
   where
      reveal = do
         revealDeckTop
         mcard <- movePort fromDeckTop toAside
         case mcard of
            Nothing   -> return $ Just ()
            Just card -> do
               if withCardType Treasure card
                  then return $ Just ()
                  else return Nothing
      toHandDiscard = do
         moveZone (fromAsideBy $ withCardType Treasure) toHand
         moveZone fromAsideAny toDiscard
moat :: DomDevice Dom => Card
moat = Card
   "Moat"
   Action
   (cost 2)
   (point 0)
   (wt $ plusCard 2)
witch :: DomDevice Dom => Card
witch = Card
   "Witch"
   Action
   (cost 5)
   (point 0)
   (wt $ plusCard 2)
spy :: DomDevice Dom => Card
spy = Card
   "Spy"
   Action
   (cost 4)
   (point 0)
   (wt $ plusCard 1 *> plusAction 1 *> (wt $ may msg (movePort fromDeckTop toDiscard)) `ifYouDo` revealDeckTop)
   where
      msg = "Discard this card? [yes/no]"
milita :: DomDevice Dom => Card
milita = Card
   "Milita"
   Action
   (cost 4)
   (point 0)
   (wt $ plusCoin 2)
village :: DomDevice Dom => Card
village = Card
   "Village"
   Action
   (cost 3)
   (point 0)
   (wt $ plusCard 1 *> plusAction 2)
bureaucrat :: DomDevice Dom => Card
bureaucrat = Card
   "Bureaucrat"
   Action
   (cost 4)
   (point 0)
   (wt $ movePort (fromSupply silver) toDeckTop)
chapel :: DomDevice Dom => Card
chapel = Card
   "Chapel"
   Action
   (cost 2)
   (point 0)
   (wt $ mapM trashFromHand =<< (doUntilBy ((<=4) . length) $ selectSomeBy cardName =<< gets hand MS.elems))

-- utils
wt :: Dom a -> b -> Dom ()
wt x y = void x

-- vim: set expandtab:
