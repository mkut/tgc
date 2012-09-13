{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Phase.MultiPlayer
   ( Phase (..)

   , phaseController

   ) where

import TableGameCombinator.Core

class Monad m => Phase m ph where
   setPhase  :: ph -> m ()
   getPhase  :: m ph
   phaseProc :: ph -> m (Maybe ph)

phaseController :: (Phase m ph, ODevice m ph)
                => ph
                -> m ()
phaseController ph = do
   setPhase ph
   tell ph
   nextPh <- phaseProc ph
   case nextPh of
      Just nextPh' -> phaseController nextPh'
      Nothing      -> return ()

-- vim: set expandtab:
