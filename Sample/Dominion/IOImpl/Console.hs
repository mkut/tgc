{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sample.Dominion.IOImpl.Console where

import Sample.Dominion.Base

import TableGameCombinator.Core
import TableGameCombinator.Tag
import TableGameCombinator.Player

import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Reader as R
import qualified Control.Monad.State.Lazy as S
import qualified Data.Label as L
import Data.List
import Data.List.Split
import Data.Array
import qualified Data.MultiSet as MS
import Data.Sequence (ViewL (..), ViewR (..), (<|), (|>))
import qualified Data.Traversable as Trav

-- runProcess
runProcess :: Dom () -> IO ()
runProcess proc = do
   S.evalStateT proc initialState

-- I/O Device instances
instance DomDevice MyDom

instance IDevice MyDom String where
   ask = lift $ lift getLine
instance IDevice MyDom [String] where
   ask = filter (/="") . splitOneOf "," <$> ask
instance IDevice MyDom YesNoInput where
   ask = do
      str <- ask
      case str of
         "yes" -> return Yes
         "no"  -> return No
         _     -> ask
instance ODevice MyDom String where
   tell x = lift $ lift $ do { putStr x; hFlush stdout }
instance ODevice MyDom [String] where
   tell = tell . (++"]\n") . ("["++) . intercalate "/"
instance ODevice MyDom DominionState where
   tell st = do
      pl <- R.ask
      let supp       =                           L.get supply       st
          deckLen    = length          $ (!pl) $ L.get deck         st
          discardLen = MS.size         $ (!pl) $ L.get discardPile  st
          coin       =                   (!pl) $ L.get coinCount    st
          action     =                   (!pl) $ L.get actionCount  st
          buy        =                   (!pl) $ L.get buyCount     st
          played     = map withoutTags $ (!pl) $ L.get playField    st
          h          =                   (!pl) $ L.get hand         st
      tell $ replicate 60 '=' ++ "\n"
      tell $ "Supply "
      forM_ (zip [0..] $ MS.toOccurList supp) $ \(i, (x, n)) -> do
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
      forM_ (zip [0..] $ MS.toList h) $ \(i, x) -> do
         when (i `mod` 5 == 0 && i /= 0) $ tell "\n       "
         tell $ show x ++ "     "
      tell "\n"
      tell $ replicate 60 '=' ++ "\n"
instance ODevice MyDom (DomPhase, Player) where
   tell (ph, pl) = tell $ whiteColor ++ "=== " ++ show pl ++ ":" ++ show ph ++ " ===" ++ defaultColor ++ "\n"
instance ODevice MyDom Log where
   tell (Draw    card) = do
      pl <- R.ask
      tell $ show pl ++ " draw a "    ++ show card ++ ".\n"
   tell (Discard card) = do
      pl <- R.ask
      tell $ show pl ++ " discard a " ++ show card ++ ".\n"
   tell (Trash   card) = do
      pl <- R.ask
      tell $ show pl ++ " trash a "   ++ show card ++ ".\n"
   tell (Play    card) = do
      pl <- R.ask
      tell $ show pl ++ " play a "    ++ show card ++ ".\n"
   tell (Reveal  card) = do
      pl <- R.ask
      tell $ show pl ++ " reveal a "  ++ show card ++ ".\n"
   tell (Buy     card) = do
      pl <- R.ask
      tell $ show pl ++ " buy a "     ++ show card ++ ".\n"
   tell (Gain    card) = do
      pl <- R.ask
      tell $ show pl ++ " gain a "    ++ show card ++ ".\n"
   tell Shuffle        = do
      pl <- R.ask
      tell $ show pl ++ " shuffle your deck.\n"
instance ODevice MyDom Int where
   tell x = do
      pl <- R.ask
      tell $ show pl ++ ": " ++ show x ++ "\n"


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
