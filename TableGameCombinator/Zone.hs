{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Zone
   ( ZoneView (..)
   , IZone (..)
   , OZone (..)

   , Top (..)
   , Bottom (..)
   , Any (..)
   , Select (..)
   ) where

import Data.List
import qualified Data.Foldable as Fold
import qualified Data.MultiSet as MS

-- ZoneView type
data ZoneView z a = Empty
                  | a :<< z

-- ZonePolicy class
class IZone z a p where
   fromView :: p a -> ZoneView z a -> z
class OZone z a p where
   view     :: p a -> z -> ZoneView z a

data Top a = Top
data Bottom a = Bottom
data Any a = Any
data Select a = Select a

-- Instances
instance IZone [a] a Top where
   fromView _ Empty    = []
   fromView _ (x:<<xs) = x:xs
instance OZone [a] a Top where
   view     _ []       = Empty
   view     _ (x:xs)   = x:<<xs

-- Construction
fromList :: IZone z a p => p a -> [a] -> z
fromList p xs = foldr (add p) (fromView p Empty) xs

toList :: OZone z a p => p a -> z -> [a]
toList p xs = unfoldr f xs
   where
      f ys = case view p ys of
         Empty   -> Nothing
         y:<<ys' -> Just (y, ys)

add :: IZone z a p => p a -> a -> z -> z
add p x xs = fromView p (x:<<xs)

tail :: OZone z a p => p a -> z -> z
tail p xs = case view p xs of
   Empty   -> xs
   _:<<xs' -> xs'

head :: OZone z a p => p a -> z -> Maybe a
head p xs = case view p xs of
   Empty -> Nothing
   x:<<_ -> Just x

-- vim: set expandtab:
