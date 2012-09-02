{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sample.Dominion.Phase where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import TableGameCombinator.Core
import TableGameCombinator.Phase

import Control.Monad
import Control.Applicative
import qualified Data.Label as L
import Data.List

instance DomDevice Dom => Phase Dom DomPhase where
   setPhase  = set phase
   getPhase  = get phase
   phaseProc ActionPhase = actionPhase
   phaseProc MoneyPhase  = moneyPhase
   phaseProc BuyPhase    = buyPhase
   phaseProc CleanUpPhase    = cleanUpPhase

-- Phase Rules
actionPhase :: DomDevice Dom => Dom (Maybe DomPhase)
actionPhase = do
   set actionCount 1
   set coinCount 0
   set buyCount 1
   doUntil actionPhaseLoop

actionPhaseLoop :: DomDevice Dom => Dom (Maybe (Maybe DomPhase))
actionPhaseLoop = do
   modify hand sort
   actionCount' <- get actionCount
   h <- get hand
   let ixs = map fst $ filter ((==Action) . cardType . snd) $ zip [0..] h
   if null ixs || actionCount' == 0
      then return (Just $ Just $ MoneyPhase)
      else do
         choose $  [ ("end", return $ Just $ Just $ MoneyPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, plusAction (-1) *> play i *> return Nothing) | i <- ixs ]

moneyPhase :: DomDevice Dom => Dom (Maybe DomPhase)
moneyPhase = doUntil moneyPhaseLoop

moneyPhaseLoop :: DomDevice Dom => Dom (Maybe (Maybe DomPhase))
moneyPhaseLoop = do
   modify hand sort
   h <- get hand
   let ixs = map fst $ filter ((==Treasure) . cardType . snd) $ zip [0..] h
   if null ixs
      then return (Just $ Just $ BuyPhase)
      else do
         choose $  [ ("end", return $ Just $ Just $ BuyPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, play i *> return Nothing) | i <- ixs ]

buyPhase :: DomDevice Dom => Dom (Maybe DomPhase)
buyPhase = doUntil buyPhaseLoop

buyPhaseLoop :: DomDevice Dom => Dom (Maybe (Maybe DomPhase))
buyPhaseLoop = do
   buyCount' <- get buyCount
   n <- gets supply length
   if buyCount' == 0
      then return (Just $ Just $ CleanUpPhase)
      else do
         choose $  [ ("end", return $ Just $ Just $ CleanUpPhase) ]
                ++ [ ("ls", tellInfo *> return Nothing) ]
                ++ [ (show i, buy i *> return Nothing) | i <- [0..n-1] ]

cleanUpPhase :: DomDevice Dom => Dom (Maybe DomPhase)
cleanUpPhase = do
   h <- get hand
   p <- get playField
   set hand []
   set playField []
   modify discardPile ((h++) . (p++))
   plusCard 5
   return $ Just ActionPhase

-- vim: set expandtab:
