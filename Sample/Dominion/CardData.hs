{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.CardData where

import Sample.Dominion.Base
import Sample.Dominion.Prim

import TableGameCombinator.Core
import TableGameCombinator.State
import TableGameCombinator.Tag
import TableGameCombinator.Zone

import Control.Monad
import Control.Applicative
import qualified Data.MultiSet as MS

copper      :: DomDevice Dom => Card
copper      = Card "Copper"      Treasure 0 0    $ \_ -> plusCoin 1
silver      :: DomDevice Dom => Card
silver      = Card "Silver"      Treasure 3 0    $ \_ -> plusCoin 2
gold        :: DomDevice Dom => Card
gold        = Card "Gold"        Treasure 6 0    $ \_ -> plusCoin 3
estate      :: DomDevice Dom => Card
estate      = Card "Estate"      Victory  2 1    $ \_ -> noAction
duchy       :: DomDevice Dom => Card
duchy       = Card "Duchy"       Victory  5 3    $ \_ -> noAction
province    :: DomDevice Dom => Card
province    = Card "Province"    Victory  8 6    $ \_ -> noAction
curse       :: DomDevice Dom => Card
curse       = Card "Curse"       Curse    0 (-1) $ \_ -> noAction
market      :: DomDevice Dom => Card
market      = Card "Market"      Action   5 0    $ \_ -> plusCard 1 *> plusAction 1 *> plusBuy 1 *> plusCoin 1
remodel     :: DomDevice Dom => Card
remodel     = Card "Remodel"     Action   4 0    $ \_ -> void $ gain `ifYouDo` select
   where
      select = chooseCard (keep trashFromHand) =<< handOps
      gain card = chooseCard gainCard =<< filter (costUpTp $ cardCost card + 2) <$> supplyOps
smithy      :: DomDevice Dom => Card
smithy      = Card "Smithy"      Action   4 0    $ \_ -> plusCard 3
moneylender :: DomDevice Dom => Card
moneylender = Card "Moneylender" Action   4 0    $ \_ -> void $ (\_ -> plusCoin 3) `ifYouDo` select
   where
      select = chooseCard (keep trashFromHand) =<< filter (==copper) <$> handOps
woodcutter  :: DomDevice Dom => Card
woodcutter  = Card "Woodcutter"  Action   3 0    $ \_ -> plusBuy 1 *> plusCoin 2
councilRoom :: DomDevice Dom => Card -- imcomplete
councilRoom = Card "CouncilRoom" Action   5 0    $ \_ -> plusCard 4 *> plusBuy 1
throneRoom  :: DomDevice Dom => Card
throneRoom  = Card "ThroneRoom"  Action   4 0    $ \t -> void $ chooseBy cardName (playCardN 2 (t+1)) =<< filter (withCardType Action) <$> handOps
laboratory  :: DomDevice Dom => Card
laboratory  = Card "Laboratory"  Action   5 0    $ \_ -> plusCard 2 *> plusAction 1
mine        :: DomDevice Dom => Card
mine        = Card "Mine"        Action   5 0    $ \_ -> void $ gain `ifYouDo` select
   where
      select = chooseCard (keep trashFromHand) =<< filter (withCardType Treasure) <$> handOps
      gain card = chooseCard gainCard =<< filter (withCardType Treasure) . filter (costUpTp $ cardCost card + 3) <$> supplyOps
workshop    :: DomDevice Dom => Card
workshop    = Card "Workshop"    Action   3 0    $ \_ -> void $ chooseCard gainCard =<< filter (costUpTp 4) <$> supplyOps
chancellor  :: DomDevice Dom => Card
chancellor  = Card "Chancellor"  Action   3 0    $ \_ -> void $ plusCoin 2 *> may msg (moveZone fromDeckTop toDiscard)
   where
      msg = "put your deck into your discard pile? [yes/no]" 
feast       :: DomDevice Dom => Card
feast       = Card "Feast"       Action   4 0    $ \t -> void $ trashTagged t *> gain
   where
      gain = chooseCard gainCard =<< filter (costUpTp 5) <$> supplyOps

-- vim: set expandtab:
