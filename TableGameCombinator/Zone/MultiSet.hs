{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TableGameCombinator.Zone.MultiSet where

import {-# SOURCE #-} TableGameCombinator.Zone

import Data.MultiSet as MS

instance Ord a => IZone (MultiSet a) a IAny where
   add _ = insert
instance Ord a => OZone (MultiSet a) a Select where
   view (Select a) xs  = if member a xs
      then a :<< delete a xs
      else NoView
instance Ord a => OZone (MultiSet a) a OAny where
   view _ xs  = if MS.null xs
      then NoView
      else let x = head $ elems xs in view (Select x) xs

-- vim: set expandtab:

