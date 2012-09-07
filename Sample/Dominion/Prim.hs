{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Prim where

import Sample.Dominion.Base
import Sample.Dominion.Card

import TableGameCombinator.Core
import TableGameCombinator.State

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import qualified Control.Monad.State.Class
import Data.List
import qualified Data.MultiSet as MS
import Data.Sequence (ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Traversable as Trav
import Data.Foldable (toList)

-- Action
play :: DomDevice Dom => Card -> Dom Card
play card = do
   tell $ "You play a " ++ show card ++ ".\n"
   modify hand (MS.delete card)
   modify playField (|>card)
   cardEffect card
   return card

playAction :: DomDevice Dom => Card -> Dom Card
playAction card = plusAction (-1) *> play card

buy :: DomDevice Dom => Card -> Dom Card
buy card = do
   tell $ "You buy a " ++ show card ++ ".\n"
   coinCount' <- get coinCount
   plusBuy (-1)
   plusCoin (-cardCost card)
   gainCard card

canBuy :: DomDevice Dom => Card -> Dom Bool
canBuy card = (cardCost card <=) <$> get coinCount

gainCard :: DomDevice Dom => Card -> Dom Card
gainCard card = do
   modify supply $ MS.delete card
   modify discardPile $ MS.insert card
   return card

gainBy :: DomDevice Dom => (Card -> Bool) -> Dom (Maybe Card)
gainBy f = chooseBy cardName gainCard =<< filter f <$> gets supply MS.distinctElems

gainUpTo :: DomDevice Dom => Int -> Dom (Maybe Card)
gainUpTo coin = gainBy ((<=coin) . cardCost)

draw :: DomDevice Dom => Dom (Maybe Card)
draw = do
   view <- gets deck Seq.viewl
   case view of
      card :< ndeck -> do
         set deck ndeck
         modify hand $ MS.insert card
         tell $ "You draw a " ++ show card ++ ".\n"
         return $ Just card
      EmptyL -> do
         emp <- gets discardPile MS.null
         if not emp
            then do
               ndeck <- gets discardPile toList
               set deck $ Seq.fromList ndeck
               set discardPile MS.empty
               shuffleDeck
               draw
            else return Nothing

trashCardFromHand :: DomDevice Dom => Card -> Dom Card
trashCardFromHand card = do
   tell $ "You trash a " ++ show card ++ ".\n"
   modify hand $ MS.delete card
   modify trashPile $ MS.insert card
   return card

trashFromHand :: DomDevice Dom => Dom (Maybe Card)
trashFromHand = trashFromHandBy $ \_ -> True

trashFromHandBy :: DomDevice Dom => (Card -> Bool) -> Dom (Maybe Card)
trashFromHandBy f = chooseBy cardName trashCardFromHand =<< filter f <$> gets hand MS.distinctElems

shuffleDeck :: DomDevice Dom => Dom ()
shuffleDeck = do
   d <- gets deck toList
   d' <- shuffleM d
   set deck $ Seq.fromList d'
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
tellInfo = do
   supply'    <- gets supply MS.toOccurList
   coin       <- get coinCount
   action     <- get actionCount
   buy        <- get buyCount
   played     <- get playField
   hand'      <- gets hand toList
   deckLen    <- gets deck Seq.length
   discardLen <- gets discardPile MS.size
   tell $ replicate 60 '=' ++ "\n"
   tell $ "Supply "
   forM_ (zip [0..] supply') $ \(i, (x, n)) -> do
      when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
      tell $ show n ++ " " ++ show x ++ "     "
   tell "\n"
   tell $ replicate 60 '-' ++ "\n"
   tell $ "deck: " ++ show deckLen ++ "   discard: " ++ show discardLen ++ "\n"
   tell $ "coin: " ++ show coin ++ "   action: " ++ show action ++ "   buy: " ++ show buy ++ "\n"
   tell $ replicate 60 '-' ++ "\n"
   tell $ "Played "
   Trav.forM (Seq.zip (Seq.fromList [0..Seq.length played - 1]) played) $ \(i, x) -> do
      when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
      tell $ show x ++ "     "
   tell "\n"
   tell $ replicate 60 '-' ++ "\n"
   tell $ "Hand   "
   forM_ (zip [0..] hand') $ \(i, x) -> do
      when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
      tell $ show x ++ "     "
   tell "\n"
   tell $ replicate 60 '=' ++ "\n"

-- vim: set expandtab:
