{-# LANGUAGE FlexibleContexts #-}
module Sample.Dominion.Prim where

import Sample.Dominion.Base

import TableGameCombinator.Core
import TableGameCombinator.Choice
import TableGameCombinator.State
import TableGameCombinator.Zone
import TableGameCombinator.Tag
import qualified TableGameCombinator.Player as P

import System.Random.Shuffle
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Applicative
import qualified Data.MultiSet as MS
import Data.Maybe
import Data.List

-- Basic
noAction :: Tag -> MyDom ()
noAction _ = return ()

plusCoin :: DomDevice MyDom
         => Int
         -> MyDom ()
plusCoin n = modify coinCount' (+n)

plusAction :: DomDevice MyDom
           => Int
           -> MyDom ()
plusAction n = modify actionCount' (+n)

plusBuy :: DomDevice MyDom
        => Int
        -> MyDom ()
plusBuy n = modify buyCount' (+n)

plusCard :: DomDevice MyDom
         => Int
         -> MyDom ()
plusCard n = replicateM_ n draw

-- Cost
cost :: Int -> Dom Int
cost x = return x

-- Victory point
vp :: Int -> MS.MultiSet Card -> Int
vp x _ = x

-- Draw a card
draw :: DomDevice MyDom
     => MyDom (Maybe Card)
draw = do
   mcard <- movePort fromDeckTop toHand
   case mcard of
      Just card -> do
         tell $ Draw card
         return $ Just card
      Nothing   -> do
         remakeDeck
         mcard' <- movePort fromDeckTop toHand
         case mcard' of
            Just card -> do
               tell $ Draw card
               return $ Just card
            Nothing -> return Nothing

revealDeckTop :: DomDevice MyDom
              => MyDom (Maybe Card)
revealDeckTop = do
   d <- get deck'
   when (null d) remakeDeck
   d' <- get deck'
   if null d'
      then return Nothing
      else do
         tell $ Reveal $ head d'
         return $ Just $ head d'

remakeDeck :: DomDevice MyDom => MyDom ()
remakeDeck = do
   moveZone fromDiscardAny toDeckTop
   shuffleDeck

discard :: DomDevice MyDom
        => Card
        -> MyDom ()
discard card = do
   tell $ Discard card
   void $ movePort (fromHand card) toDiscard

discardDeckTop :: DomDevice MyDom
               => MyDom (Maybe Card)
discardDeckTop = do -- 順番逆にする
   mcard <- movePort fromDeckTop toDiscard
   when (isJust mcard) $ tell $ Discard $ fromJust mcard
   return mcard

discardRevealed :: DomDevice MyDom
                => Card
                -> MyDom ()
discardRevealed card = do
   tell $ Discard card
   void $ movePort (fromAsideBy (==card)) toDiscard

-- Play a card
playTagged :: DomDevice MyDom
           => Tag
           -> MyDom (Maybe TCard)
playTagged t = do
   mtcard <- getTagged t playField'
   case mtcard of
      Nothing   -> return Nothing
      Just tcard -> do
         tell $ Play (withoutTags tcard)
         cardEffect (withoutTags tcard) t
         return $ Just tcard

playCard :: DomDevice MyDom
         => Tag
         -> Card
         -> MyDom ()
playCard = playCardN 1

playCardN :: DomDevice MyDom
          => Int
          -> Tag
          -> Card
          -> MyDom ()
playCardN n t card = do
   movePortWith (tag t . withNoTags) (fromHand card) toPlay
   replicateM_ n $ playTagged t
   modify playField' $ map (untag t)

playAction :: DomDevice MyDom
           => Card
           -> MyDom ()
playAction card = do
   plusAction (-1)
   playCard 0 card

playMoney :: DomDevice MyDom
          => Card
          -> MyDom ()
playMoney = playCard 0

-- Trash a card
trashFromHand :: DomDevice MyDom
              => Card
              -> MyDom ()
trashFromHand card = do
   tell $ Trash card
   void $ movePort (fromHand card) toTrash

trashTagged :: DomDevice MyDom
            => Tag
            -> MyDom (Maybe Card)
trashTagged t = movePortWith withoutTags (fromPlayTagged t) toTrash

trashRevealed :: DomDevice MyDom
              => Card
              -> MyDom ()
trashRevealed card = do
   tell $ Trash card
   void $ movePort (fromAsideBy (==card)) toTrash

-- Gain a card
gainCard :: DomDevice MyDom
         => Card
         -> MyDom ()
gainCard card = do
   tell $ Gain card
   void $ movePort (fromSupply card) toDiscard

buy :: DomDevice MyDom
    => Card -> MyDom ()
buy card = do
   tell $ Buy card
   plusBuy (-1)
   cost <- lift $ cardCost card
   plusCoin (-cost)
   void $ gainCard card

gainCardToHand :: DomDevice MyDom
         => Card
         -> MyDom ()
gainCardToHand card = do
   tell $ Gain card
   void $ movePort (fromSupply card) toHand

gainCardFromTrash :: DomDevice MyDom
                  => Card
                  -> MyDom ()
gainCardFromTrash card = do
   tell $ Gain card
   void $ movePort (fromTrash card) toDiscard

-- Shuffle a deck
shuffleDeck :: DomDevice MyDom => MyDom ()
shuffleDeck = do
   tell $ Shuffle
   d <- get deck'
   d' <- shuffleM d
   set deck' d'

-- Clean up
cleanUp :: DomDevice MyDom => MyDom ()
cleanUp = do
   moveZone fromHandAny toDiscard
   moveZoneWith withoutTags fromPlay toDiscard
   return ()

-- Status
canBuy :: DomDevice MyDom
       => Card
       -> MyDom Bool
canBuy card = liftM2 (<=) (lift $ cardCost card) (get coinCount')

-- Options
handOps :: MyDom [Card]
handOps = gets hand' MS.distinctElems

supplyOps :: Dom [Card]
supplyOps = gets supply MS.distinctElems

asideOps :: Dom [Card]
asideOps = gets aside MS.distinctElems

-- Choice
chooseCard :: DomDevice MyDom
           => (Card -> MyDom a)
           -> [Card]
           -> MyDom (Maybe a)
chooseCard = chooseBy cardName

-- Function for card filters
costUpTo :: Int -> Card -> Dom Bool
costUpTo x card = (<=x) <$> cardCost card

costUpToDiff :: Card -> Int -> Card -> Dom Bool
costUpToDiff card' x card = flip costUpTo card . (+x) =<< cardCost card'

withCardType :: CardType -> Card -> Bool
withCardType t = (==t) . cardType

-- Finalize
finished :: Dom Bool
finished = do
   flag1 <- isNothing . find ((=="Province") . cardName) <$> gets supply MS.distinctElems
   flag2 <- (<=14) . length <$> gets supply MS.distinctElems
   return (flag1 || flag2)

countScore :: MyDom Int
countScore = do
   moveZone fromDeckTop toHand
   moveZoneWith withoutTags fromPlay toHand
   moveZone fromDiscardAny toHand
   moveZone fromAsideAny toHand
   myHand' <- P.my hand'
   d <- get myHand'
   return $ sum $ map cardVP (MS.elems d) <*> [d]

-- IO (only)
tellInfo :: DomDevice MyDom => MyDom ()
tellInfo = tell =<< getAll

-- vim: set expandtab:
