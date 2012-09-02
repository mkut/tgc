{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Phase
   ( Phase (..)

   , phaseController

   ) where

import TableGameCombinator.Core

class Monad m => Phase m ph where
   setPhase  :: ph -> m ()
   getPhase  :: m ph
   phaseProc :: ph -> m (Maybe ph)

phaseController :: (Show ph, Phase m ph, ODevice m String)
                => ph
                -> m ()
phaseController ph = do
   setPhase ph
   tell $ "::: " ++ show ph ++ " :::\n"
   nextPh <- phaseProc ph
   case nextPh of
      Just nextPh' -> phaseController nextPh'
      Nothing      -> return ()

-- vim: set expandtab:
