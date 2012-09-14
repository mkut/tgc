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
import Control.Monad.Trans.Class (lift)
import Control.Applicative
import qualified Data.MultiSet as MS

copper :: DomDevice MyDom => Card
copper = Card
   "Copper"
   Treasure
   (cost 0)
   (vp 0)
   (wt $ plusCoin 1)
silver :: DomDevice MyDom => Card
silver = Card
   "Silver"
   Treasure
   (cost 3)
   (vp 0)
   (wt $ plusCoin 2)
gold :: DomDevice MyDom => Card
gold  = Card
   "Gold"
   Treasure
   (cost 6)
   (vp 0)
   (wt $ plusCoin 3)
estate :: DomDevice MyDom => Card
estate = Card
   "Estate"
   Victory
   (cost 2)
   (vp 1)
   noAction
duchy :: DomDevice MyDom => Card
duchy = Card
   "Duchy"
   Victory
   (cost 5)
   (vp 3)
   noAction
province :: DomDevice MyDom => Card
province = Card
   "Province"
   Victory
   (cost 8)
   (vp 6)
   noAction
curse :: DomDevice MyDom => Card
curse = Card
   "Curse"
   Curse
   (cost 0)
   (vp (-1))
   noAction
market :: DomDevice MyDom => Card
market = Card
   "Market"
   Action
   (cost 5)
   (vp 0)
   (wt $ plusCard 1 *> plusAction 1 *> plusBuy 1 *> plusCoin 1)
remodel :: DomDevice MyDom => Card
remodel = Card
   "Remodel"
   Action
   (cost 4)
   (vp 0)
   (wt $ gain `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< handOps
      gain card = chooseCard gainCard =<< lift (filterM (costUpToDiff card 2) =<< supplyOps)
smithy :: DomDevice MyDom => Card
smithy  = Card
   "Smithy"
   Action
   (cost 4)
   (vp 0)
   (wt $ plusCard 3)
moneylender :: DomDevice MyDom => Card
moneylender = Card
   "Moneylender"
   Action
   (cost 4)
   (vp 0)
   (wt $ (\_ -> plusCoin 3) `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< filter (==copper) <$> handOps
woodcutter :: DomDevice MyDom => Card
woodcutter = Card
   "Woodcutter"
   Action
   (cost 3)
   (vp 0)
   (wt $ plusBuy 1 *> plusCoin 2)
councilRoom :: DomDevice MyDom => Card
councilRoom = Card
   "CouncilRoom"
   Action
   (cost 5)
   (vp 0)
   (wt $ plusCard 4 *> plusBuy 1 *> eachOpponentsDo (plusCard 1))
throneRoom :: DomDevice MyDom => Card
throneRoom = Card
   "ThroneRoom"
   Action
   (cost 4)
   (vp 0)
   (\t -> void $ chooseBy cardName (playCardN 2 (t+1)) =<< filter (withCardType Action) <$> handOps)
laboratory :: DomDevice MyDom => Card
laboratory = Card
   "Laboratory"
   Action
   (cost 5)
   (vp 0)
   (wt $ plusCard 2 *> plusAction 1)
mine :: DomDevice MyDom => Card
mine = Card
   "Mine"
   Action
   (cost 5)
   (vp 0)
   (wt $ gain `ifYouDo` select)
   where
      select = chooseCard (keep trashFromHand) =<< typeFilter =<< handOps
      gain card = chooseCard gainCardToHand =<< typeFilter =<< lift (costFilter =<< supplyOps)
         where
            costFilter = filterM (costUpToDiff card 3)
      typeFilter = filterM (return . withCardType Treasure)
workshop :: DomDevice MyDom => Card
workshop = Card
   "Workshop"
   Action
   (cost 3)
   (vp 0)
   (wt $ chooseCard gainCard =<< lift (filterM (costUpTo 4) =<< supplyOps))
chancellor :: DomDevice MyDom => Card
chancellor = Card
   "Chancellor"
   Action
   (cost 3)
   (vp 0)
   (wt $ plusCoin 2 *> may msg (moveZone fromDeckTop toDiscard))
   where
      msg = "Put your deck into your discard pile? [yes/no]" 
feast :: DomDevice MyDom => Card
feast = Card
   "Feast"
   Action
   (cost 4)
   (vp 0)
   $ \t -> void $ trashTagged t *> gain
   where
      gain = chooseCard gainCard =<< lift (filterM (costUpTo 5) =<< supplyOps)
festival :: DomDevice MyDom => Card
festival = Card
   "Festival"
   Action
   (cost 5)
   (vp 0)
   (wt $ plusAction 2 *> plusBuy 1 *> plusCoin 2)
library :: DomDevice MyDom => Card
library = Card
   "Library"
   Action
   (cost 5)
   (vp 0)
   (wt $ doWhile drawReveal *> moveZone fromAsideAny toDiscard)
   where
      drawReveal = do
         n <- gets hand' MS.size
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
cellor :: DomDevice MyDom => Card
cellor = Card
   "Cellor"
   Action
   (cost 2)
   (vp 0)
   (wt $ plusAction 1 *> (plusCard =<< length <$> select))
   where
      select = chooseSomeBy cardName (keep discard) =<< gets hand' MS.elems
gardens :: DomDevice MyDom => Card
gardens = Card
   "Gardens"
   Victory
   (cost 4)
   ((`div`10) . MS.size)
   noAction
thief :: DomDevice MyDom => Card
thief = Card
   "Thief"
   Action
   (cost 4)
   (vp 0)
   (wt $ do
      y <- you
      eachOpponentsDo $ do
         replicateM_ 2 $ revealDeckTop *> movePort fromDeckTop toAside
         lift $ heDo y $ chooseCard choose1 =<< filterM (return . withCardType Treasure) =<< lift asideOps
         mapM_ discardRevealed =<< gets aside' MS.elems)
   where
      choose1 card = do
         trashRevealed card
         choose2 card
      choose2 card = may msg $ gainCardFromTrash card
      msg = "Gain this card? [yes/no]"
adventurer :: DomDevice MyDom => Card
adventurer = Card
   "Adventurer"
   Action
   (cost 6)
   (vp 0)
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
moat :: DomDevice MyDom => Card
moat = Card
   "Moat"
   Action
   (cost 2)
   (vp 0)
   (wt $ plusCard 2)
witch :: DomDevice MyDom => Card
witch = Card
   "Witch"
   Action
   (cost 5)
   (vp 0)
   (wt $ eachOpponentsDo $ gainCard curse)
spy :: DomDevice MyDom => Card
spy = Card
   "Spy"
   Action
   (cost 4)
   (vp 0)
   (wt $ do
      y <- you
      plusCard 1
      plusAction 1
      eachPlayersDo $ (\_ -> lift $ heDo y $ may msg discardDeckTop) `ifYouDo` revealDeckTop)
   where
      msg = "Discard this card? [yes/no]"
milita :: DomDevice MyDom => Card
milita = Card
   "Milita"
   Action
   (cost 4)
   (vp 0)
   (wt $ do
      eachOpponentsDo $ doUntilBy (<=3) $ do
         chooseCard discard =<< handOps
         gets hand' MS.size)
village :: DomDevice MyDom => Card
village = Card
   "Village"
   Action
   (cost 3)
   (vp 0)
   (wt $ plusCard 1 *> plusAction 2)
bureaucrat :: DomDevice MyDom => Card
bureaucrat = Card
   "Bureaucrat"
   Action
   (cost 4)
   (vp 0)
   (wt $ do
      movePort (fromSupply silver) toDeckTop
      eachOpponentsDo $ do
         mcard <- act1
         case mcard of
            Just card -> act2 card
            Nothing   -> act3)
   where
      act1 = selectBy cardName =<< filterM (return . withCardType Victory) =<< gets hand' MS.distinctElems
      act2 card = do
         tell $ Reveal card
         movePort (fromHand card) toDeckTop
         return ()
      act3 = mapM_ (tell . Reveal) =<< handOps
chapel :: DomDevice MyDom => Card
chapel = Card
   "Chapel"
   Action
   (cost 2)
   (vp 0)
   (wt $ mapM trashFromHand =<< (doUntilBy ((<=4) . length) $ selectSomeBy cardName =<< gets hand' MS.elems))

-- utils
wt :: MyDom a -> b -> MyDom ()
wt x y = void x

-- vim: set expandtab:
