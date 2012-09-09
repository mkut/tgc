module TableGameCombinator.Tag where

import TableGameCombinator.State

import Data.Set hiding (map)
import Data.List (find)

-- Tagged type
type Tag = Int
type Tagged a = (a, Set Tag)

tag :: Tag -> Tagged a -> Tagged a
tag t (x, ts) = (x, insert t ts)

untag :: Tag -> Tagged a -> Tagged a
untag t (x, ts) = (x, delete t ts)

tagged :: Tag -> Tagged a -> Bool
tagged t (_, ts) = member t ts

withoutTags :: Tagged a -> a
withoutTags = fst

withTags :: Set Tag -> a -> Tagged a
withTags ts x = (x, ts)

withNoTags :: a -> Tagged a
withNoTags = withTags empty

-- TaggableZone type
class TaggableZone z where
   tagZone    :: Tag -> (Tagged a -> Bool) -> z (Tagged a) -> z (Tagged a)
   untagZone  :: Tag -> z (Tagged a) -> z (Tagged a)
   findTagged :: Tag -> z (Tagged a) -> Maybe (Tagged a)

tagZone' :: TaggableZone z
         => Tag
         -> (a -> Bool)
         -> z (Tagged a)
         -> z (Tagged a)
tagZone' t f = tagZone t (f . withoutTags)

instance TaggableZone [] where
   tagZone t f xs = case break f xs of
      (_ , []  ) -> xs
      (ys, z:zs) -> ys ++ [tag t z] ++ zs
   untagZone t    = map (untag t)
   findTagged t   = find (tagged t)

-- Tagging process
addTag :: (TaggableZone z, RecordMonadState s m l)
       => Tag
       -> (Tagged a -> Bool)
       -> l s (z (Tagged a))
       -> m ()
addTag t f lens = modify lens $ tagZone t f

removeTag :: (TaggableZone z, RecordMonadState s m l)
          => Tag
          -> l s (z (Tagged a))
          -> m ()
removeTag t lens = modify lens $ untagZone t

getTagged :: (TaggableZone z, RecordMonadState s m l)
          => Tag
          -> l s (z (Tagged a))
          -> m (Maybe (Tagged a))
getTagged t lens = gets lens $ findTagged t

-- vim: set expandtab:
