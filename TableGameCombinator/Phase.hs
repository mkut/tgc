{-# LANGUAGE MultiParamTypeClasses #-}
module TableGameCombinator.Phase
   ( Phase (..)

   , phaseController

   ) where

import TableGameCombinator.Core

class Phase gs ph where
   setPhase  :: ph -> Process gs ()
   getPhase  :: Process gs ph
   phaseProc :: ph -> Process gs (Maybe ph)

phaseController :: (Show ph, Phase gs ph) => ph -> Process gs ()
phaseController ph = do
   setPhase ph
   tell $ "::: " ++ show ph ++ " :::\n"
   nextPh <- phaseProc ph
   case nextPh of
      Just nextPh' -> phaseController nextPh'
      Nothing      -> return ()

-- vim: set expandtab:
