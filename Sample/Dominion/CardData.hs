{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.CardData where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import Control.Monad
import Control.Applicative

import TableGameCombinator.Core

copper      = Card "Copper"      Treasure 0 0    $ plusCoin 1
silver      = Card "Silver"      Treasure 3 0    $ plusCoin 2
gold        = Card "Gold"        Treasure 6 0    $ plusCoin 3
estate      = Card "Estate"      Victory  2 1    $ return ()
duchy       = Card "Duchy"       Victory  5 3    $ return ()
province    = Card "Province"    Victory  8 6    $ return ()
curse       = Card "Curse"       Curse    0 (-1) $ return ()
market      :: DomDevice Dom => Card
market      = Card "Market"      Action   5 0    $ plusCard 1 *> plusAction 1 *> plusBuy 1 *> plusCoin 1
remodel     :: DomDevice Dom => Card
remodel     = Card "Remodel"     Action   4 0    $ void $ ifYouDo (gainUpTo . (+2) . cardCost) trashFromHand
smithy      :: DomDevice Dom => Card
smithy      = Card "Smithy"      Action   4 0    $ plusCard 3
moneylender :: DomDevice Dom => Card
moneylender = Card "Moneylender" Action   4 0    $ void $ ifYouDo (\_ -> plusCoin 3) $ trashFromHandBy (==copper)
woodcutter  :: DomDevice Dom => Card
woodcutter  = Card "Woodcutter"  Action   3 0    $ plusBuy 1 *> plusCoin 2
councilRoom :: DomDevice Dom => Card -- imcomplete
councilRoom = Card "CouncilRoom" Action   5 0    $ plusCard 4 *> plusBuy 1

-- vim: set expandtab:
