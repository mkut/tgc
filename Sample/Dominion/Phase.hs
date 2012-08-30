{-# LANGUAGE MultiParamTypeClasses #-}
module Sample.Dominion.Phase where

import Sample.Dominion.Base
import Sample.Dominion.PrimEffect
import Sample.Dominion.IO

import TableGameCombinator.Core
import TableGameCombinator.Phase

import Control.Monad
import Control.Applicative
import qualified Data.Label as L
import Data.List

instance Phase DominionState DomPhase where
   setPhase  = modify . L.set phase
   getPhase  = gets (L.get phase)
   phaseProc ActionPhase = actionPhase
   phaseProc MoneyPhase  = moneyPhase
   phaseProc BuyPhase    = buyPhase
   phaseProc EndPhase    = endPhase

-- Phase Rules
actionPhase :: Dom (Maybe DomPhase)
actionPhase = do
   modify (L.set actionCount 1)
   modify (L.set coinCount 0)
   modify (L.set buyCount 1)
   doUntil actionPhaseLoop

actionPhaseLoop :: Dom (Maybe (Maybe DomPhase))
actionPhaseLoop = do
   modify (L.modify hand sort)
   actionCount' <- gets (L.get actionCount)
   h <- gets (L.get hand)
   let ixs = map fst $ filter ((==Action) . cardType . snd) $ zip [0..] h
   if null ixs || actionCount' == 0
      then return (Just $ Just $ MoneyPhase)
      else do
         tell $ "[" ++ intercalate "/" (["end", "ls"] ++ (map show ixs)) ++ "]\n"
         choose $  [ ("end", return $ Just $ Just $ MoneyPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, plusAction (-1) *> play i *> return Nothing) | i <- ixs ]

moneyPhase :: Dom (Maybe DomPhase)
moneyPhase = doUntil moneyPhaseLoop

moneyPhaseLoop :: Dom (Maybe (Maybe DomPhase))
moneyPhaseLoop = do
   modify (L.modify hand sort)
   h <- gets (L.get hand)
   let ixs = map fst $ filter ((==Treasure) . cardType . snd) $ zip [0..] h
   if null ixs
      then return (Just $ Just $ BuyPhase)
      else do
         tell $ "[" ++ intercalate "/" (["end", "ls"] ++ (map show ixs)) ++ "]\n"
         choose $  [ ("end", return $ Just $ Just $ BuyPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, play i *> return Nothing) | i <- ixs ]

buyPhase :: Dom (Maybe DomPhase)
buyPhase = doUntil buyPhaseLoop

buyPhaseLoop :: Dom (Maybe (Maybe DomPhase))
buyPhaseLoop = do
   buyCount' <- gets (L.get buyCount)
   n <- gets (length . L.get supply)
   if buyCount' == 0
      then return (Just $ Just $ EndPhase)
      else do
         tell $ "[" ++ intercalate "/" (["end", "ls"] ++ (map show [0..n-1])) ++ "]\n"
         choose $  [ ("end", return $ Just $ Just $ EndPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, buy i *> return Nothing) | i <- [0..n-1] ]

endPhase :: Dom (Maybe DomPhase)
endPhase = do
   h <- gets (L.get hand)
   p <- gets (L.get playField)
   modify (L.set hand [])
   modify (L.set playField [])
   modify (L.modify discardPile ((h++) . (p++)))
   plusCard 5
   return $ Just ActionPhase

-- vim: set expandtab:
