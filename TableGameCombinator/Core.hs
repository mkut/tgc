{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Core
   ( IDevice (..)
   , ODevice (..)

   , YesNoInput (..)
   , may
   , ifYouDo
   , choose
   , chooseBy
   , doUntil
   ) where

import Control.Monad
import Data.List
import qualified Data.Traversable as Trav

-- I/O Device
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

ifYouDo :: Monad m => (a -> m b) -> m (Maybe a) -> m (Maybe b)
ifYouDo f = (Trav.mapM f =<<)

choose :: (Eq i, IDevice m i, ODevice m [i]) => [(i, m a)] -> m a
choose ops = do
   tell $ map fst ops
   res <- ask
   case lookup res ops of
      Just proc -> proc
      Nothing   -> choose ops

chooseBy :: (Eq i, IDevice m i, ODevice m [i]) => (a -> i) -> (a -> m a) -> [a] -> m a
chooseBy f g ops = choose $ zip (map f ops) (map g ops)

doUntil :: Monad m => m (Maybe a) -> m a
doUntil proc = do
   x <- proc
   case x of
      Just x' -> return x'
      Nothing -> doUntil proc

-- vim: set expandtab:
