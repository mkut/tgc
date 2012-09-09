{-# LANGUAGE TupleSections #-}
module TableGameCombinator.Zone where

import TableGameCombinator.Core
import TableGameCombinator.State

import Control.Monad
import Data.List

-- Insert/Delete type
type InsertPort z a = a -> z -> z
type DeletePort z a = z -> Maybe (a, z)

deletePort :: (z -> Maybe a) -> (z -> z) -> DeletePort z a
deletePort f g z = liftM (,g z) $ f z

listDeletePort :: DeletePort [a] a
listDeletePort = deletePort (find $ \_ -> True) tail

-- Zone I/O port type
type ZoneIPort l s z a = (l s z, InsertPort z a)
type ZoneDPort l s z a = (l s z, DeletePort z a)

putPort :: (RecordMonadState s m l)
        => ZoneIPort l s z a
        -> a
        -> m ()
putPort (lens, ins) x = modify lens (ins x)

takePort :: (RecordMonadState s m l)
         => ZoneDPort l s z a
         -> m (Maybe a)
takePort (lens, del) = do
   setRet `ifYouDo` gets lens del
   where
      setRet (x, xs) = do
         set lens xs
         return x

movePort :: (RecordMonadState s m l)
         => ZoneDPort l s z1 a
         -> ZoneIPort l s z2 a
         -> m (Maybe a)
movePort = movePortWith id

movePortWith :: (RecordMonadState s m l)
             => (a -> b)
             -> ZoneDPort l s z1 a
             -> ZoneIPort l s z2 b
             -> m (Maybe b)
movePortWith f op ip = putPortRet `ifYouDo` takePort op
   where
      putPortRet x = let y = f x in do
         putPort ip y
         return y

moveZone :: (RecordMonadState s m l)
            => ZoneDPort l s z1 a
            -> ZoneIPort l s z2 a
            -> m [a]
moveZone op ip = doWhile $ movePort op ip

moveZoneWith :: (RecordMonadState s m l)
            => (a -> b)
            -> ZoneDPort l s z1 a
            -> ZoneIPort l s z2 b
            -> m [b]
moveZoneWith f op ip = doWhile $ movePortWith f op ip

-- vim: set expandtab:
