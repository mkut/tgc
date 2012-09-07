{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TableGameCombinator.Zone.MultiSet where

import TableGameCombinator.Zone

import Data.MultiSet

instance Ord a => IZone (MultiSet a) a Any where
   fromView _ Empty    = empty
   fromView _ (x:<<xs) = insert x xs
instance Ord a => OZone (MultiSet a) a Select where
   view (Select a) xs  = if member a xs
      then a :<< delete a xs
      else Empty

-- vim: set expandtab:

