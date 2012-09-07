{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TableGameCombinator.Zone.Sequence where

import {-# SOURCE #-} TableGameCombinator.Zone

import Data.Sequence

instance IZone (Seq a) a Top where
   add _ = (<|)
instance OZone (Seq a) a Top where
   view     _ xs       = case viewl xs of
      EmptyL -> NoView
      x:<xs' -> x:<<xs'
instance IZone (Seq a) a Bottom where
   add _ = flip (|>)
instance OZone (Seq a) a Bottom where
   view     _ xs       = case viewr xs of
      EmptyR -> NoView
      xs':>x -> x:<<xs'

-- vim: set expandtab:
