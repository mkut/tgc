{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Choice
   ( choose
   , chooseBy
   , choose'
   , chooseBy'
   , chooseSome
   , chooseSomeBy
   ) where

import TableGameCombinator.Core

import Control.Exception.Base
import Control.Applicative
import Data.List
import Data.Maybe

choose :: (Eq i, IDevice m i, ODevice m [i], Functor m)
       => [(i, m a)]
       -> m (Maybe a)
choose []   = return Nothing
choose [op] = Just <$> snd op
choose ops  = do
   tell $ map fst ops
   res <- ask
   case lookup res ops of
      Just proc -> Just <$> proc
      Nothing   -> choose ops

chooseBy :: (Eq i, IDevice m i, ODevice m [i], Functor m)
         => (a -> i)
         -> (a -> m b)
         -> [a]
         -> m (Maybe b)
chooseBy f g ops = choose $ zip (map f ops) (map g ops)

choose' :: (Eq i, IDevice m i, ODevice m [i], Functor m)
        => [(i, m a)]
        -> m a
choose' ops = fromJust <$> choose ops

chooseBy' :: (Eq i, IDevice m i, ODevice m [i], Functor m)
          => (a -> i)
          -> (a -> m b)
          -> [a]
          -> m b
chooseBy' f g ops = fromJust <$> chooseBy f g ops

chooseSome :: (Eq i, IDevice m [i], ODevice m [i], Functor m) => [(i, m a)] -> m [a]
chooseSome []  = return []
chooseSome ops = do
   tell $ map fst ops
   res <- ask
   let ret = snd $ mapAccumL f ops res
   if all isJust ret
      then sequence $ map fromJust ret
      else chooseSome ops
   where
      f ops' r = case lookup r ops' of
         Just proc -> (deleteBy (\x y -> fst x == fst y) (r, proc) ops', Just proc)
         Nothing   -> ([], Nothing)

chooseSomeBy :: (Eq i, IDevice m [i], ODevice m [i], Functor m) => (a -> i) -> (a -> m b) -> [a] -> m [b]
chooseSomeBy f g ops = chooseSome $ zip (map f ops) (map g ops)

-- vim: set expandtab:
