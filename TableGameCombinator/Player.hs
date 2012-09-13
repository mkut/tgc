{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module TableGameCombinator.Player
   ( PlayerLens (..)
   
   , its
   , fromArrayLens
   , fromLens
   , my

   ) where

import TableGameCombinator.Core (ODevice (..))
import TableGameCombinator.State

import Control.Monad
import Control.Monad.Reader
import Data.Array
import Data.Label (Lens, (:->))
import qualified Data.Label as L

data PlayerLens pl s a = PlayerLens
   { getOf :: pl -> s -> a
   , setOf :: pl -> a -> s -> s
   }

{-
class MultiPlayer m pl where
   allPlayers :: m [pl]

instance (MultiPlayer m pl, ODevice (ReaderT pl m) o) => ODevice m o where
   tell o = do
      pls <- allPlayers
      forM_ pls $ runReaderT (tell o)
-}

instance RecordMonadState s m (Lens (->)) => RecordMonadState s (ReaderT pl m) (PlayerLens pl) where
   get l = ReaderT $ \r -> get $ its r l
   set l x = ReaderT $ \r -> set (its r l) x

its :: pl -> PlayerLens pl s a -> (s :-> a)
its r l = L.lens (getOf l r) (setOf l r)

fromArrayLens :: (Ix pl) => (s :-> Array pl a) -> PlayerLens pl s a
fromArrayLens l = PlayerLens f g
   where
      f r s = L.get l s ! r
      g r e = L.modify l (// [(r, e)])

fromLens :: (s :-> a) -> PlayerLens pl s a
fromLens l = PlayerLens (\_ -> L.get l) (\_ -> L.set l)

my :: Monad m => PlayerLens pl s a -> ReaderT pl m (s :-> a)
my l = do
   r <- ask
   return $ L.lens (getOf l r) (setOf l r)

-- vim: set expandtab:
