{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Core
   ( IDevice (..)
   , ODevice (..)

   , YesNoInput (..)
   , may
   , ifYouDo
   , ifYouDont
   , doUntil
   , doUntilBy
   , doWhile
   , doWhileBy
   , keep
   
   ) where

import Control.Monad
import Control.Applicative
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

doUntil :: Monad m => m (Maybe a) -> m a
doUntil proc = do
   x <- proc
   case x of
      Just x' -> return x'
      Nothing -> doUntil proc

doUntilBy :: Monad m => (a -> Bool) -> m a -> m a
doUntilBy f = doUntil . liftM (maybeBy f)

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile proc = do
   mx <- proc
   case mx of
      Just x  -> liftM (x:) $ doWhile proc
      Nothing -> return []

doWhileBy :: Monad m => (a -> Bool) -> m a -> m [a]
doWhileBy f = doWhile . liftM (maybeBy f)

maybeBy :: (a -> Bool) -> a -> Maybe a
maybeBy f x = if f x then Just x else Nothing

keep :: Monad m => (a -> m b) -> a -> m a
keep f x = do
   f x
   return x

-- vim: set expandtab:
