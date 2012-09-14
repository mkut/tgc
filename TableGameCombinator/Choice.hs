{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Choice
   ( choose
   , chooseBy
   , choose'
   , chooseBy'
   , chooseSome
   , chooseSomeBy

   , select
   , selectBy
   , selectSome
   , selectSomeBy
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

select :: (Eq i, IDevice m i, ODevice m [i], Functor m)
       => [i]
       -> m (Maybe i)
select ops  = choose $ map (\op -> (op, return op)) ops

selectBy :: (Eq i, IDevice m i, ODevice m [i], Functor m)
         => (a -> i)
         -> [a]
         -> m (Maybe a)
selectBy f ops = chooseBy f return ops

selectSome :: (Eq i, IDevice m [i], ODevice m [i], Functor m)
           => [i]
           -> m [i]
selectSome ops = chooseSome $ map (\op -> (op, return op)) ops

selectSomeBy :: (Eq i, IDevice m [i], ODevice m [i], Functor m)
             => (a -> i)
             -> [a]
             -> m [a]
selectSomeBy f ops = chooseSomeBy f return ops

-- vim: set expandtab:
