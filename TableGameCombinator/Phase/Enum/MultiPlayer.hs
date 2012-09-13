{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TableGameCombinator.Phase.Enum.MultiPlayer
   ( MultiPlayerEnumPhase (..)
   ) where

import qualified TableGameCombinator.Phase as P

import Control.Monad.Reader

class (Monad m, Enum ph, Enum pl) => MultiPlayerEnumPhase m ph pl where
   setPhase    :: (ph, pl) -> m ()
   getPhase    :: m (ph, pl)
   phaseProc   :: (ph, pl) -> ReaderT pl m ()
   hasFinished :: (ph, pl) -> m Bool

instance MultiPlayerEnumPhase m ph pl => P.Phase m (ph, pl) where
   setPhase = setPhase
   getPhase = getPhase
   phaseProc pp@(ph, pl) = do
      runReaderT (phaseProc pp) pl
      finished <- hasFinished pp
      if finished
         then return Nothing
         else if enumLast ph
            then return $ Just $ (toEnum 0, succ' pl)
            else return $ Just $ (succ ph, pl)
      where
         enumLast x = null $ tail $ enumFrom x
         succ' x = if enumLast x then toEnum 0 else succ x

-- vim: set expandtab:
