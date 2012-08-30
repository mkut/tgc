module Sample.Dominion.IO
   ( tellHand
   , tellInfo
   ) where

import Sample.Dominion.Base
import Sample.Dominion.Card

import TableGameCombinator.Core

import Control.Monad
import qualified Data.Label as L

tellHand :: Dom ()
tellHand = do
   gets (L.get hand) >>= tell . (++"\n") . show

tellInfo :: Dom ()
tellInfo = do
   supply'    <- gets (L.get supply)
   coin       <- gets (L.get coinCount)
   action     <- gets (L.get actionCount)
   buy        <- gets (L.get buyCount)
   hand'      <- gets (L.get hand)
   deckLen    <- gets (length . L.get deck)
   discardLen <- gets (length . L.get discardPile)
   tell $ replicate 60 '=' ++ "\n"
   forM_ (zip [0..] supply') $ \(i, (x, n)) -> do
      when (i `mod` 5 == 0 && i /= 0) $ tell "\n"
      tell $ show n ++ " " ++ show x ++ "     "
   tell "\n"
   tell $ replicate 60 '-' ++ "\n"
   tell $ "deck: " ++ show deckLen ++ "   discard: " ++ show discardLen ++ "\n"
   tell $ "coin: " ++ show coin ++ "   action: " ++ show action ++ "   buy: " ++ show buy ++ "\n"
   tell $ replicate 60 '-' ++ "\n"
   forM_ (zip [0..] hand') $ \(i, x) -> do
      when (i `mod` 5 == 0 && i /= 0) $ tell "\n"
      tell $ show x ++ "     "
   tell "\n"
   tell $ replicate 60 '=' ++ "\n"

-- vim: set expandtab:
