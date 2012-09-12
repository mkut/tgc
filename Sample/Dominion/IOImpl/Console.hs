{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sample.Dominion.IOImpl.Console where

import Sample.Dominion.Base

import TableGameCombinator.Core

import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import qualified Control.Monad.State.Lazy as S
import qualified Data.Label as L
import Data.List
import Data.List.Split
import qualified Data.MultiSet as MS
import Data.Sequence (ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Traversable as Trav

-- runProcess
runProcess :: Dom () -> IO ()
runProcess proc = do
   S.evalStateT proc initialState

-- I/O Device instances
instance DomDevice Dom

instance IDevice Dom String where
   ask = lift getLine
instance IDevice Dom [String] where
   ask = filter (/="") . splitOneOf "," <$> ask
instance IDevice Dom YesNoInput where
   ask = do
      str <- ask
      case str of
         "yes" -> return Yes
         "no"  -> return No
         _     -> ask
instance ODevice Dom String where
   tell x = lift $ do { putStr x; hFlush stdout }
instance ODevice Dom [String] where
   tell = tell . (++"]\n") . ("["++) . intercalate "/"
instance ODevice Dom DominionState where
   tell st = do
      tell $ replicate 60 '=' ++ "\n"
      tell $ "Supply "
      forM_ (zip [0..] $ MS.toOccurList supply') $ \(i, (x, n)) -> do
         when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
         tell $ show n ++ " " ++ show x ++ "     "
      tell "\n"
      tell $ replicate 60 '-' ++ "\n"
      tell $ "deck: " ++ show deckLen ++ "   discard: " ++ show discardLen ++ "\n"
      tell $ "coin: " ++ show coin ++ "   action: " ++ show action ++ "   buy: " ++ show buy ++ "\n"
      tell $ replicate 60 '-' ++ "\n"
      tell $ "Played "
      Trav.forM (zip ([0..length played - 1]) played) $ \(i, x) -> do
         when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
         tell $ show x ++ "     "
      tell "\n"
      tell $ replicate 60 '-' ++ "\n"
      tell $ "Hand   "
      forM_ (zip [0..] $ MS.toList hand') $ \(i, x) -> do
         when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
         tell $ show x ++ "     "
      tell "\n"
      tell $ replicate 60 '=' ++ "\n"
      where
         supply'    =           L.get supply      st
         deckLen    = length  $ L.get deck        st
         discardLen = MS.size $ L.get discardPile st
         coin       =           L.get coinCount   st
         action     =           L.get actionCount st
         buy        =           L.get buyCount    st
         played     =           L.get playField   st
         hand'      =           L.get hand        st
instance ODevice Dom DomPhase where
   tell x = tell $ whiteColor ++ "=== " ++ show x ++ " ===" ++ defaultColor ++ "\n"
instance ODevice Dom Log where
   tell (Draw    card) = tell $ "You draw a "    ++ show card ++ ".\n"
   tell (Discard card) = tell $ "You discard a " ++ show card ++ ".\n"
   tell (Trash   card) = tell $ "You trash a "   ++ show card ++ ".\n"
   tell (Play    card) = tell $ "You play a "    ++ show card ++ ".\n"
   tell (Reveal  card) = tell $ "You reveal a "  ++ show card ++ ".\n"
   tell (Buy     card) = tell $ "You buy a "     ++ show card ++ ".\n"
   tell (Gain    card) = tell $ "You gain a "    ++ show card ++ ".\n"
   tell Shuffle        = tell $ "You shuffle your deck.\n"
instance ODevice Dom Int where
   tell x = tell $ show x ++ "\n"

-- Show Card instance
instance Show Card where
   show x =  colorStringOfCardType (cardType x)
          ++ cardName x
          ++ defaultColor

colorStringOfCardType :: CardType -> String
colorStringOfCardType Treasure = yellowColor
colorStringOfCardType Victory  = greenColor
colorStringOfCardType Action   = whiteColor
colorStringOfCardType Curse    = purpleColor

-- Terminal color
yellowColor  = "\ESC[1;33m"
whiteColor   = "\ESC[1;37m"
greenColor   = "\ESC[1;32m"
purpleColor  = "\ESC[1;35m"
defaultColor = "\ESC[1;m"

-- vim: set expandtab:
