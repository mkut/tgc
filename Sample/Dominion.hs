{-# LANGUAGE TemplateHaskell #-}
module Sample.Dominion
   ( main
   , initialState
   ) where

import TableGameCombinator.Core

import System.Random.Shuffle
import Control.Monad
import Control.Applicative
import Data.Label (mkLabel)
import qualified Data.Label as L
import Data.List

import Debug.Trace

-- Components
data Card = Copper
          | Estate
          deriving (Eq, Ord)
instance Show Card where
   show Copper = "\ESC[1;33mCopper\ESC[1;m"
   show Estate = "\ESC[1;32mEstate\ESC[1;m"

data Phase = ActionPhase
           | MoneyPhase
           | BuyPhase
           | EndPhase
           | OtherPhase
           deriving (Show, Eq)

-- GameState
data DominionState = DS
   { _phase       :: Phase
   , _deck        :: [Card]
   , _hand        :: [Card]
   , _playField   :: [Card]
   , _discardPile :: [Card]
   , _trashPile   :: [Card]
   , _supply      :: [(Card, Int)]
   , _actionCount :: Int
   , _coinCount   :: Int
   , _buyCount    :: Int
   }
mkLabel ''DominionState

initialState :: DominionState
initialState = DS
   { _phase       = OtherPhase
   , _deck        = replicate 7 Copper ++ replicate 3 Estate
   , _hand        = []
   , _playField   = []
   , _discardPile = []
   , _trashPile   = []
   , _supply      = [(Copper, 53), (Estate, 4)]
   , _actionCount = 1
   , _coinCount   = 0
   , _buyCount    = 1
   }

-- Types
type Dom  = Process DominionState

-- Rules
main :: Dom ()
main = endPhase

-- Phase
phaseCheck :: Phase -> Dom () -> Dom ()
phaseCheck ph ini = do
   prevPhase <- gets (L.get phase)
   when (prevPhase /= ph) $ do
      tell $ "::: " ++ show ph ++ ":::\n"
      modify (L.set phase ph)
      ini

actionPhase :: Dom ()
actionPhase = do
   phaseCheck ActionPhase $ do
      modify (L.set actionCount 1)
      modify (L.set coinCount 0)
      modify (L.set buyCount 1)
   modify (L.modify hand sort)
   n <- gets (length . L.get hand)
   tell $ "[end/ls/0-" ++ show (n-1) ++ "]\n"
   choose $  [ ("end", moneyPhase), ("ls", tellHand *> actionPhase)]
          ++ [ (show i, play i *> actionPhase) | i <- [0..n-1] ]

moneyPhase :: Dom ()
moneyPhase = do
   phaseCheck MoneyPhase (return ())
   modify (L.modify hand sort)
   n <- gets (length . L.get hand)
   tell $ "[end/ls/0-" ++ show (n-1) ++ "]\n"
   choose $  [ ("end", buyPhase), ("ls", tellHand *> moneyPhase)]
          ++ [ (show i, play i *> moneyPhase) | i <- [0..n-1] ]

buyPhase :: Dom ()
buyPhase = do
   phaseCheck BuyPhase (return ())
   n <- gets (length . L.get supply)
   tell $ "[end/ls/0-" ++ show (n-1) ++ "]\n"
   choose $  [ ("end", endPhase), ("ls", tellHand *> buyPhase)]
          ++ [ (show i, buy i *> buyPhase) | i <- [0..n-1] ]

endPhase :: Dom ()
endPhase = do
   phaseCheck EndPhase $ do
      h <- gets (L.get hand)
      p <- gets (L.get playField)
      modify (L.set hand [])
      modify (L.set playField [])
      modify (L.modify discardPile ((h++) . (p++)))
      replicateM_ 5 draw
   actionPhase

-- Action
play :: Int -> Dom Card
play i = do
   card <- gets ((!!i) . L.get hand)
   modify (L.modify hand (delete card))
   modify (L.modify playField (card:))
   return card

buy :: Int -> Dom Card
buy i = do
   former <- gets (take i . L.get supply)
   (card, n) <- gets ((!!i) . L.get supply)
   latter <- gets (drop (i+1) . L.get supply)
   when (n /= 0) $ do
      modify (L.set supply $ former ++ [(card, n-1)] ++ latter)
      modify (L.modify discardPile (card:))
   return card

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

-- IO (only) Action
tellHand :: Dom ()
tellHand = do
   gets (L.get hand) >>= tell . (++"\n") . show

-- vim: set expandtab:
