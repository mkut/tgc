{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Core
   ( IDevice (..)
   , ODevice (..)

   , YesNoInput (..)
   , may
   , choose
   , doUntil
   ) where

import Control.Monad
import Data.List

-- IO Device
class Monad m => IDevice m i where
   ask  :: m i
class Monad m => ODevice m o where
   tell :: o -> m ()

-- Combinators
data YesNoInput = Yes
                | No

may :: (ODevice m o, IDevice m YesNoInput) => o -> m a -> m (Maybe a)
may msg proc = do
   tell msg
   res <- ask
   case res of
      Yes -> liftM Just proc
      No  -> return Nothing

choose :: (Eq i, IDevice m i, ODevice m [i]) => [(i, m a)] -> m a
choose alts = do
   tell $ map fst alts
   res <- ask
   case lookup res alts of
      Just proc -> proc
      Nothing   -> choose alts

doUntil :: Monad m => m (Maybe a) -> m a
doUntil proc = do
   x <- proc
   case x of
      Just x' -> return x'
      Nothing -> doUntil proc

-- vim: set expandtab:
