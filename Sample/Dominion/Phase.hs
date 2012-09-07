{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sample.Dominion.Phase where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Phase

import Control.Monad
import Control.Applicative
import qualified Data.Label as L
import Data.List
import Data.Maybe
import qualified Data.MultiSet as MS
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

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
   actionCount' <- get actionCount
   available <- gets hand $ filter ((==Action) . cardType) . MS.distinctElems
   if null available || actionCount' == 0
      then return (Just $ Just $ MoneyPhase)
      else do
         (fromJust <$>) . choose $
               [ ("end", return $ Just $ Just $ MoneyPhase) ]
            ++ [ ("ls", tellInfo *> return Nothing) ]
            ++ [ (cardName card, playAction card *> return Nothing) | card <- available ]

moneyPhase :: DomDevice Dom => Dom (Maybe DomPhase)
moneyPhase = doUntil moneyPhaseLoop

moneyPhaseLoop :: DomDevice Dom => Dom (Maybe (Maybe DomPhase))
moneyPhaseLoop = do
   available <- gets hand $ filter ((==Treasure) . cardType) . MS.distinctElems
   if null available
      then return (Just $ Just $ BuyPhase)
      else do
         (fromJust <$>) . choose $
               [ ("end", return $ Just $ Just $ BuyPhase) ]
            ++ [ ("ls", tellInfo *> return Nothing) ]
            ++ [ (cardName card, play card *> return Nothing) | card <- available ]

buyPhase :: DomDevice Dom => Dom (Maybe DomPhase)
buyPhase = doUntil buyPhaseLoop

buyPhaseLoop :: DomDevice Dom => Dom (Maybe (Maybe DomPhase))
buyPhaseLoop = do
   buyCount' <- get buyCount
   supply' <- gets supply MS.distinctElems
   available <- filterM canBuy supply'
   if buyCount' == 0
      then return (Just $ Just $ CleanUpPhase)
      else do
         (fromJust <$>) . choose $
               [ ("end", return $ Just $ Just $ CleanUpPhase) ]
            ++ [ ("ls", tellInfo *> return Nothing) ]
            ++ [ (cardName card, buy card *> return Nothing) | card <- available ]

cleanUpPhase :: DomDevice Dom => Dom (Maybe DomPhase)
cleanUpPhase = do
   h <- get hand
   modify discardPile $ MS.union h
   set hand MS.empty
   p <- get playField
   modify discardPile $ MS.union $ MS.fromList $ toList p
   set playField Seq.empty
   plusCard 5
   return $ Just ActionPhase

-- vim: set expandtab:
