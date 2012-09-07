{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TableGameCombinator.State
   ( RecordMonadState (..)
   , gets
   , modify
   , getAll
   ) where

import Control.Monad
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Lazy as S
import Data.Label (Lens)
import qualified Data.Label as L

class MonadState s m => RecordMonadState s m lens where
   get :: lens s a -> m a
   set :: lens s a -> a -> m ()

instance MonadState s m => RecordMonadState s m (Lens (->)) where
   get     = S.gets . L.get
   set k x = S.modify $ L.set k x

gets :: RecordMonadState s m lens => lens s a -> (a -> b) -> m b
gets k f = liftM f $ get k

modify :: RecordMonadState s m lens => lens s a -> (a -> a) -> m ()
modify k f = set k =<< gets k f

getAll :: MonadState s m => m s
getAll = S.get

-- vim: set expandtab:
