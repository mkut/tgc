module Sample.Dominion.PrimEffect where

import TableGameCombinator.Core
import Sample.Dominion.Base
import Sample.Dominion.Card

import System.Random.Shuffle
import Control.Monad
import qualified Data.Label as L
import Data.List

-- Action
play :: Int -> Dom Card
play i = do
   card <- gets ((!!i) . L.get hand)
   modify (L.modify hand (delete card))
   modify (L.modify playField (card:))
   cardEffect card
   return card

buy :: Int -> Dom (Maybe Card)
buy i = do
   former <- gets (take i . L.get supply)
   (card, n) <- gets ((!!i) . L.get supply)
   latter <- gets (drop (i+1) . L.get supply)
   coinCount' <- gets (L.get coinCount)
   if n /= 0 && coinCount' >= cardCost card
      then do
         plusBuy (-1)
         modify (L.set supply $ former ++ [(card, n-1)] ++ latter)
         modify (L.modify discardPile (card:))
         return $ Just card
      else do
         tell $ "you can't!\n"
         return Nothing

draw :: Dom (Maybe Card)
draw = do
   emp <- gets (null . L.get deck)
   emp' <- gets (null . L.get discardPile)
   case (emp, emp') of
      (False, _) -> do
         card <- gets (head . L.get deck)
         modify (L.modify deck tail)
         modify (L.modify hand (card:))
         tell $ "You draw a " ++ show card ++ ".\n"
         return $ Just card
      (True, False) -> do
         d <- gets (L.get discardPile)
         modify (L.set deck d)
         modify (L.set discardPile [])
         shuffleDeck
         draw
      (True, True) -> return Nothing

shuffleDeck :: Dom ()
shuffleDeck = do
   d <- gets (L.get deck)
   d' <- shuffleM d
   modify (L.set deck d')
   tell $ "You shuffle your deck.\n"

plusCoin :: Int -> Dom ()
plusCoin n = modify (L.modify coinCount (+n))

plusAction :: Int -> Dom ()
plusAction n = modify (L.modify actionCount (+n))

plusBuy :: Int -> Dom ()
plusBuy n = modify (L.modify buyCount (+n))

plusCard :: Int -> Dom ()
plusCard n = replicateM_ n draw

-- vim: set expandtab:
