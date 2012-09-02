{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Prim where

import Sample.Dominion.Base
import Sample.Dominion.Card

import TableGameCombinator.Core

import System.Random.Shuffle
import Control.Monad
import Data.List

-- Action
play :: DomDevice Dom => Int -> Dom Card
play i = do
   card <- gets hand (!!i)
   modify hand (delete card)
   modify playField (card:)
   cardEffect card
   return card

buy :: DomDevice Dom => Int -> Dom (Maybe Card)
buy i = do
   former <- gets supply (take i)
   (card, n) <- gets supply (!!i)
   latter <- gets supply (drop (i+1))
   coinCount' <- get coinCount
   if n /= 0 && coinCount' >= cardCost card
      then do
         plusBuy (-1)
         set supply $ former ++ [(card, n-1)] ++ latter
         modify discardPile (card:)
         return $ Just card
      else do
         tell $ "you can't!\n"
         return Nothing

draw :: DomDevice Dom => Dom (Maybe Card)
draw = do
   emp <- gets deck null
   emp' <- gets discardPile null
   case (emp, emp') of
      (False, _) -> do
         card <- gets deck head
         modify deck tail
         modify hand (card:)
         tell $ "You draw a " ++ show card ++ ".\n"
         return $ Just card
      (True, False) -> do
         d <- get discardPile
         set deck d
         set discardPile []
         shuffleDeck
         draw
      (True, True) -> return Nothing

shuffleDeck :: DomDevice Dom => Dom ()
shuffleDeck = do
   d <- get deck
   d' <- shuffleM d
   set deck d'
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
   supply'    <- get supply
   coin       <- get coinCount
   action     <- get actionCount
   buy        <- get buyCount
   played     <- get playField
   hand'      <- get hand
   deckLen    <- gets deck length
   discardLen <- gets discardPile length
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
   forM_ (zip [0..] played) $ \(i, x) -> do
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
