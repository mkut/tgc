module TableGameCombinator.Zone.State
   ( putZone
   , takeZone
   , moveZone
   , moveZoneAll
   ) where

import TableGameCombinator.Core
import TableGameCombinator.Zone
import TableGameCombinator.State

import Control.Monad

putZone :: (IZone z a p, RecordMonadState s m lens)
        => lens s z
        -> p a
        -> a
        -> m ()
putZone lens p x = modify lens (add p x)

takeZone :: (OZone z a p, RecordMonadState s m lens)
         => lens s z
         -> p a
         -> m (Maybe a)
takeZone lens p = do
   res <- gets lens (view p)
   case res of
      NoView -> return Nothing
      x:<<xs -> do
         set lens xs
         return $ Just x

moveZone :: (IZone z2 a p2, OZone z1 a p1, RecordMonadState s m lens)
         => lens s z1
         -> p1 a
         -> lens s z2
         -> p2 a
         -> m (Maybe a)
moveZone lens1 p1 lens2 p2 = ifYouDo f $ takeZone lens1 p1
   where
      f x = do
         putZone lens2 p2 x
         return x

moveZoneAll :: (IZone z2 a p2, OZone z1 a p1, RecordMonadState s m lens)
            => lens s z1
            -> p1 a
            -> lens s z2
            -> p2 a
            -> m ()
moveZoneAll lens1 p1 lens2 p2 = do
   (z2, z1) <- gets lens1 (toList p1)
   modify lens2 $ fromList p2 z2
   set lens1 z1

-- vim: set expandtab:
