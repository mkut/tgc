{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module TableGameCombinator.Zone.Sequence where

import TableGameCombinator.Zone

import Data.Sequence

instance IZone (Seq a) a Top where
   fromView _ Empty    = empty
   fromView _ (x:<<xs) = x<|xs
instance OZone (Seq a) a Top where
   view     _ xs       = case viewl xs of
      EmptyL -> Empty
      x:<xs' -> x:<<xs'
instance IZone (Seq a) a Bottom where
   fromView _ Empty    = empty
   fromView _ (x:<<xs) = xs|>x
instance OZone (Seq a) a Bottom where
   view     _ xs       = case viewr xs of
      EmptyR -> Empty
      xs':>x -> x:<<xs'

-- vim: set expandtab:
