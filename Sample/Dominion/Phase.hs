{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Sample.Dominion.Phase where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import TableGameCombinator.Core
import TableGameCombinator.Choice
import TableGameCombinator.State
import TableGameCombinator.Phase.Enum

import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import qualified Data.MultiSet as MS

instance DomDevice Dom => EnumPhase Dom DomPhase where
   setPhase  = set phase
   getPhase  = getPhase
   phaseProc ActionPhase  = actionPhase
   phaseProc MoneyPhase   = moneyPhase
   phaseProc BuyPhase     = buyPhase
   phaseProc CleanUpPhase = cleanUpPhase
   hasFinished CleanUpPhase = checkFinished
   hasFinished _            = return False

-- Phase rules
actionPhase :: DomDevice Dom => Dom ()
actionPhase = do
   set actionCount 1
   set coinCount 0
   set buyCount 1
   doUntil actionPhaseLoop

actionPhaseLoop :: DomDevice Dom => Dom (Maybe ())
actionPhaseLoop = do
   actionCount' <- get actionCount
   ops <- filter (withCardType Action) <$> handOps
   if null ops || actionCount' == 0
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ (cardName card, playAction card *> return Nothing) | card <- ops ]

moneyPhase :: DomDevice Dom => Dom ()
moneyPhase = doUntil moneyPhaseLoop

moneyPhaseLoop :: DomDevice Dom => Dom (Maybe ())
moneyPhaseLoop = do
   ops <- filter (withCardType Treasure) <$> handOps
   all <- filter (withCardType Treasure) <$> gets hand MS.elems
   if null ops
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ ("all", mapM_ playMoney all *> return Nothing) ]
                   ++ [ (cardName card, playMoney card *> return Nothing) | card <- ops ]

buyPhase :: DomDevice Dom => Dom ()
buyPhase = doUntil buyPhaseLoop

buyPhaseLoop :: DomDevice Dom => Dom (Maybe ())
buyPhaseLoop = do
   buyCount' <- get buyCount
   supply' <- gets supply MS.distinctElems
   ops <- filterM canBuy supply'
   if buyCount' == 0
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ (cardName card, buy card *> return Nothing) | card <- ops ]

cleanUpPhase :: DomDevice Dom => Dom ()
cleanUpPhase = do
   cleanUp
   plusCard 5

-- Phase options
endPhase :: DomDevice Dom
         => (String, Dom (Maybe ()))
endPhase = ("end", return $ Just ())

showInfo :: DomDevice Dom
         => (String, Dom (Maybe ()))
showInfo = ("ls", tellInfo *> return Nothing)

-- Checking flags
checkFinished :: Dom Bool
checkFinished = do
   flag1 <- isNothing . find ((=="Province") . cardName) <$> gets supply MS.distinctElems
   flag2 <- (<=14) . length <$> gets supply MS.distinctElems
   return (flag1 || flag2)

-- vim: set expandtab:
