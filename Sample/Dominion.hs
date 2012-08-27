module Sample.Dominion where

import Data.IORef
import Control.Monad
import TableGameCombinator.Core
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.Random.Shuffle

-- Components
type Card = Int
type Player = Int

system :: Player
system = 0

you :: Player
you = 1

-- Types
type Dom  = Process DominionState

instance GameState DominionState where
   initialState = do
      initialDecks <- replicateM 2 $ newGameVar []
      initialHands <- replicateM 2 $ newGameVar []
      initialSubject <- newGameVar system
      return $ DominionState
         { deck = \i -> initialDecks !! i
         , hand = \i -> initialHands !! i
         , subject = initialSubject
         }

-- QuasiQuoter
data DominionState = DominionState
   { deck    :: Player -> GameVar [Card]
   , hand    :: Player -> GameVar [Card]
   , subject :: GameVar Player
   }
getMyDeck :: Dom [Card]
getMyDeck = Process $ \gs -> do
   p <- readIORef $ subject gs
   readIORef (deck gs p)

updateMyDeck :: ([Card] -> [Card]) -> Dom ()
updateMyDeck f = Process $ \gs -> do
   p <- readIORef $ subject gs
   modifyIORef (deck gs p) f

setMyDeck :: [Card] -> Dom ()
setMyDeck x = Process $ \gs -> do
   p <- readIORef $ subject gs
   writeIORef (deck gs p) x

updateMyHand :: ([Card] -> [Card]) -> Dom ()
updateMyHand f = Process $ \gs -> do
   p <- readIORef $ subject gs
   modifyIORef (hand gs p) f

getPlayer :: Dom Player
getPlayer = Process $ \gs -> readIORef (subject gs)

setPlayer :: Player -> Dom ()
setPlayer x = Process $ \gs -> do
   writeIORef (subject gs) x

-- Rules
draw :: Dom Card
draw = do
   (card:_) <- getMyDeck
   updateMyDeck tail
   updateMyHand (card:)
   p <- getPlayer
   msgProcess $ show p ++ " draws a " ++ show card ++ "."
   return card

shuffleDeck :: Dom ()
shuffleDeck = do
   deck <- getMyDeck
   deck' <- shuffleM deck
   setMyDeck deck'
   p <- getPlayer
   msgProcess $ show p ++ " shuffles him/her deck."

initGame :: Dom ()
initGame = do
   setMyDeck [0..9]
   may $ shuffleDeck
   replicateM_ 5 draw

initialProcess :: Dom ()
initialProcess = do
   setPlayer you
   initGame
   setPlayer system

-- vim: set expandtab:
