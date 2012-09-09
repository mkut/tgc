{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Core
   ( IDevice (..)
   , ODevice (..)

   , YesNoInput (..)
   , may
   , ifYouDo
   , ifYouDont
   , choose
   , chooseBy
   , doUntil
   , doWhile
   , keep
   ) where

import Control.Monad
import Control.Applicative
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

ifYouDont :: Monad m => m b -> m (Maybe a) -> m (Maybe b)
ifYouDont f x = do
   x' <- x
   case x' of
      Nothing -> liftM Just f
      Just _  -> return Nothing

choose :: (Eq i, IDevice m i, ODevice m [i], Functor m) => [(i, m a)] -> m (Maybe a)
choose []   = return Nothing
choose [op] = Just <$> snd op
choose ops  = do
   tell $ map fst ops
   res <- ask
   case lookup res ops of
      Just proc -> Just <$> proc
      Nothing   -> choose ops

chooseBy :: (Eq i, IDevice m i, ODevice m [i], Functor m) => (a -> i) -> (a -> m b) -> [a] -> m (Maybe b)
chooseBy f g ops = choose $ zip (map f ops) (map g ops)

doUntil :: Monad m => m (Maybe a) -> m a
doUntil proc = do
   x <- proc
   case x of
      Just x' -> return x'
      Nothing -> doUntil proc

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile proc = do
   mx <- proc
   case mx of
      Just x  -> liftM (x:) $ doWhile proc
      Nothing -> return []

keep :: Monad m => (a -> m b) -> a -> m a
keep f x = do
   f x
   return x

-- vim: set expandtab:
