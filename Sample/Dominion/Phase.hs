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
import TableGameCombinator.Phase.Enum.MultiPlayer

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import qualified Data.MultiSet as MS

instance DomDevice MyDom => MultiPlayerEnumPhase Dom DomPhase Player where
   setPhase (ph, pl) = set phase ph *> set activePlayer pl
   getPhase = liftM2 (,) (get phase) (get activePlayer)
   phaseProc (ActionPhase , _) = actionPhase
   phaseProc (MoneyPhase  , _) = moneyPhase
   phaseProc (BuyPhase    , _) = buyPhase
   phaseProc (CleanUpPhase, _) = cleanUpPhase
   hasFinished (CleanUpPhase, _) = checkFinished
   hasFinished (_           , _) = return False

-- Phase rules
actionPhase :: DomDevice MyDom => MyDom ()
actionPhase = do
   set actionCount' 1
   set coinCount' 0
   set buyCount' 1
   doUntil actionPhaseLoop

actionPhaseLoop :: DomDevice MyDom => MyDom (Maybe ())
actionPhaseLoop = do
   action <- get actionCount'
   ops <- filter (withCardType Action) <$> handOps
   if null ops || action == 0
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ (cardName card, playAction card *> return Nothing) | card <- ops ]

moneyPhase :: DomDevice MyDom => MyDom ()
moneyPhase = doUntil moneyPhaseLoop

moneyPhaseLoop :: DomDevice MyDom => MyDom (Maybe ())
moneyPhaseLoop = do
   ops <- filter (withCardType Treasure) <$> handOps
   all <- filter (withCardType Treasure) <$> gets hand' MS.elems
   if null ops
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ ("all", mapM_ playMoney all *> return Nothing) ]
                   ++ [ (cardName card, playMoney card *> return Nothing) | card <- ops ]

buyPhase :: DomDevice MyDom => MyDom ()
buyPhase = doUntil buyPhaseLoop

buyPhaseLoop :: DomDevice MyDom => MyDom (Maybe ())
buyPhaseLoop = do
   b <- get buyCount'
   ops <- filterM canBuy =<< lift supplyOps
   if b == 0
      then return $ Just ()
      else choose' $  [ endPhase, showInfo ]
                   ++ [ (cardName card, buy card *> return Nothing) | card <- ops ]

cleanUpPhase :: DomDevice MyDom => MyDom ()
cleanUpPhase = do
   cleanUp
   plusCard 5

-- Phase options
endPhase :: DomDevice MyDom
         => (String, MyDom (Maybe ()))
endPhase = ("end", return $ Just ())

showInfo :: DomDevice MyDom
         => (String, MyDom (Maybe ()))
showInfo = ("ls", tellInfo *> return Nothing)

-- Checking flags
checkFinished :: DomDevice MyDom => Dom Bool
checkFinished = do
   fin <- finished
   when fin $ do
      mapM_ (runReaderT (tell =<< countScore)) [Player1 .. Player2]
   return fin
   where

-- vim: set expandtab:
