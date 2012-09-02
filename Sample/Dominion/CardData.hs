{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.CardData where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import Control.Applicative

copper   = Card "Copper"   Treasure 0 0    $ plusCoin 1
silver   = Card "Silver"   Treasure 3 0    $ plusCoin 2
gold     = Card "Gold"     Treasure 6 0    $ plusCoin 3
estate   = Card "Estate"   Victory  2 1    $ return ()
duchy    = Card "Duchy"    Victory  5 3    $ return ()
province = Card "Province" Victory  8 6    $ return ()
curse    = Card "Curse"    Curse    0 (-1) $ return ()
market :: DomDevice Dom => Card
market   = Card "Market"   Action   5 0    $ plusCard 1 *> plusAction 1 *> plusBuy 1 *> plusCoin 1

-- vim: set expandtab:
