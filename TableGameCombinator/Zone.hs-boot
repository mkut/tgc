{-# LANGUAGE MultiParamTypeClasses #-}
module TableGameCombinator.Zone where
   
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

-- vim: set expandtab:
