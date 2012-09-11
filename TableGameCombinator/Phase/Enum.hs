{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TableGameCombinator.Phase.Enum
   ( EnumPhase (..)
   ) where

import qualified TableGameCombinator.Phase as P

class (Monad m, Enum ph) => EnumPhase m ph where
   setPhase    :: ph -> m ()
   getPhase    :: m ph
   phaseProc   :: ph -> m ()
   hasFinished :: ph -> m Bool

instance EnumPhase m ph => P.Phase m ph where
   setPhase     = setPhase
   getPhase     = getPhase
   phaseProc ph = do
      phaseProc ph
      finished <- hasFinished ph
      if finished
         then return Nothing
         else return $ Just $ succ' ph
      where
         succ' x = if null $ tail $ enumFrom x then toEnum 0 else succ x

-- vim: set expandtab:
