{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Prim where

import Sample.Dominion.Base

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Zone
import TableGameCombinator.Tag

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import qualified Data.MultiSet as MS
import Data.Foldable (toList)

-- Basic
noAction :: Dom ()
noAction = return ()

plusCoin :: DomDevice Dom
         => Int
         -> Dom ()
plusCoin n = modify coinCount (+n)

plusAction :: DomDevice Dom
           => Int
           -> Dom ()
plusAction n = modify actionCount (+n)

plusBuy :: DomDevice Dom
        => Int
        -> Dom ()
plusBuy n = modify buyCount (+n)

plusCard :: DomDevice Dom
         => Int
         -> Dom ()
plusCard n = replicateM_ n draw

-- Draw a card
draw :: DomDevice Dom
     => Dom (Maybe Card)
draw = do
   mcard <- movePort fromDeckTop toHand
   case mcard of
      Just card -> do
         tell $ Draw card
         return $ Just card
      Nothing   -> do
         moveZone fromDiscardAny toDeckTop
         shuffleDeck
         mcard' <- movePort fromDeckTop toHand
         case mcard' of
            Just card -> do
               tell $ Draw card
               return $ Just card
            Nothing -> return Nothing

discard :: DomDevice Dom
        => Card
        -> Dom ()
discard card = do
   tell $ Discard card
   void $ movePort (fromHand card) toDiscard

-- Play a card
playTagged :: DomDevice Dom
           => Tag
           -> Dom (Maybe TCard)
playTagged t = do
   mtcard <- getTagged t playField
   case mtcard of
      Nothing   -> return Nothing
      Just tcard -> do
         tell $ Play (withoutTags tcard)
         cardEffect (withoutTags tcard) t
         return $ Just tcard

playCard :: DomDevice Dom
         => Tag
         -> Card
         -> Dom ()
playCard = playCardN 1

playCardN :: DomDevice Dom
          => Int
          -> Tag
          -> Card
          -> Dom ()
playCardN n t card = do
   movePortWith (tag t . withNoTags) (fromHand card) toPlay
   replicateM_ n $ playTagged t
   modify playField $ map (untag t)

playAction :: DomDevice Dom
           => Card
           -> Dom ()
playAction card = do
   plusAction (-1)
   playCard 0 card

playMoney :: DomDevice Dom
          => Card
          -> Dom ()
playMoney = playCard 0

-- Trash a card
trashFromHand :: DomDevice Dom
              => Card
              -> Dom ()
trashFromHand card = do
   tell $ Trash card
   void $ movePort (fromHand card) toTrash

trashTagged :: DomDevice Dom
            => Tag
            -> Dom (Maybe Card)
trashTagged t = movePortWith withoutTags (fromPlayTagged t) toTrash

-- Gain a card
gainCard :: DomDevice Dom
         => Card
         -> Dom ()
gainCard card = do
   tell $ Gain card
   void $ movePort (fromSupply card) toDiscard

buy :: DomDevice Dom
    => Card -> Dom ()
buy card = do
   tell $ Buy card
   coinCount' <- get coinCount
   plusBuy (-1)
   plusCoin (-cardCost card)
   void $ gainCard card

gainCardToHand :: DomDevice Dom
         => Card
         -> Dom ()
gainCardToHand card = do
   tell $ Gain card
   void $ movePort (fromSupply card) toHand

-- Shuffle a deck
shuffleDeck :: DomDevice Dom => Dom ()
shuffleDeck = do
   tell $ Shuffle
   d <- get deck
   d' <- shuffleM d
   set deck d'

-- Status
canBuy :: DomDevice Dom
       => Card
       -> Dom Bool
canBuy card = (cardCost card <=) <$> get coinCount

-- Options
handOps :: Dom [Card]
handOps = gets hand MS.distinctElems

supplyOps :: Dom [Card]
supplyOps = gets supply MS.distinctElems

-- Choice
chooseCard :: DomDevice Dom
           => (Card -> Dom a)
           -> [Card]
           -> Dom (Maybe a)
chooseCard = chooseBy cardName

-- Function for card filters
costUpTp :: Int -> Card -> Bool
costUpTp x = (<=x) . cardCost

withCardType :: CardType -> Card -> Bool
withCardType t = (==t) . cardType

-- IO (only)
tellInfo :: DomDevice Dom => Dom ()
tellInfo = tell =<< getAll

-- vim: set expandtab:
