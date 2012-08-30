module Sample.Dominion.Card
   (
   ) where

import Sample.Dominion.Base

colorStringOfCardType :: CardType -> String
colorStringOfCardType Treasure = "\ESC[1;33m"
colorStringOfCardType Victory  = "\ESC[1;32m"
colorStringOfCardType Action   = "\ESC[1;37m"
colorStringOfCardType Curse    = "\ESC[1;35m"

instance Eq Card where
   x == y = cardName x == cardName y
instance Ord Card where
   compare x y = compare (cardName x) (cardName y)
instance Show Card where
   show x =  colorStringOfCardType (cardType x)
          ++ cardName x
          ++ colorDefault
      where
         colorDefault  = "\ESC[1;m"

-- vim: set expandtab:
