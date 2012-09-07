{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module TableGameCombinator.Zone
   ( ZoneView (..)
   , IZone (..)
   , OZone (..)

   , Top (..)
   , Bottom (..)
   , IAny (..)
   , OAny (..)
   , Select (..)
   , Peek (..)

   , fromList
   , toList
   ) where

import Prelude hiding (span, takeWhile, dropWhile)
import qualified Data.Foldable as Fold
import qualified Data.MultiSet as MS
import TableGameCombinator.Zone.Sequence
import TableGameCombinator.Zone.MultiSet

-- ZoneView type
data ZoneView z a = NoView
                  | a :<< z

-- I/O Zone port class
class IZone z a p where
   add  :: p a -> a -> z -> z
class OZone z a p where
   view :: p a -> z -> ZoneView z a

data Top a = Top
data Bottom a = Bottom
data IAny a = IAny
data OAny a = OAny
data Select a = Select a
data Peek p a = Peek (p a)

-- Instances
instance IZone [a] a Top where
   add _ x xs = x:xs
instance OZone [a] a Top where
   view _ []       = NoView
   view _ (x:xs)   = x:<<xs
instance OZone z a p => OZone z a (Peek p) where
   view (Peek p) xs = case view p xs of
      NoView -> NoView
      x:<<_  -> x:<<xs

---------- Zone.hs-boot ----------

-- Construction
fromList :: IZone z a p => p a -> [a] -> z -> z
fromList p xs zero = foldr (add p) zero xs

toList :: OZone z a p => p a -> z -> ([a], z)
toList p xs = span p (\_ -> True) xs

span :: OZone z a p => p a -> (a -> Bool) -> z -> ([a], z)
span p f xs = let (rr, z) = span' p f xs [] in (reverse rr, z)

span' :: OZone z a p => p a -> (a -> Bool) -> z -> [a] -> ([a], z)
span' p f xs r1 = case view p xs of
   NoView  -> (r1, xs)
   x:<<xs' -> span' p f xs' (x:r1)

takeWhile :: OZone z a p => p a -> (a -> Bool) -> z -> [a]
takeWhile p f xs = fst $ span p f xs

dropWhile :: OZone z a p => p a -> (a -> Bool) -> z -> z
dropWhile p f xs = snd $ span p f xs
   

-- vim: set expandtab:
