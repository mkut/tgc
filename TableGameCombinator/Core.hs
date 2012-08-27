{-# LANGUAGE RankNTypes #-}
module TableGameCombinator.Core
   ( Game
   , GameVar
   , newGameVar
   , readGameVar
   , writeGameVar
   , modifyGameVar

   , GameState (..)

   , Process (..)

   , runProcess

   , msgProcess
   , inputProcess

   , may
   ) where

import System.Random.Shuffle
import Control.Monad
import Control.Monad.Random.Class
import Data.IORef

-- Game Monad
type Game    = IO
type GameVar = IORef

-- GameVar Manipulations
newGameVar :: a -> Game (GameVar a)
newGameVar = newIORef

readGameVar :: GameVar a -> Game a
readGameVar = readIORef

writeGameVar :: GameVar a -> a -> Game ()
writeGameVar = writeIORef

modifyGameVar :: GameVar a -> (a -> a) -> Game ()
modifyGameVar = modifyIORef

-- GameState class
class GameState gs where
   initialState :: Game gs

-- Game Process
newtype Process gs a = Process (GameState gs => gs -> Game a)
instance Monad (Process gs) where
   a >>= k  = Process $
      \gs -> let Process a' = a in
             do
                Process ka' <- fmap k (a' gs)
                ka' gs
   return x = Process $
      \_ -> return x

-- Execution
runProcess :: GameState gs => gs -> Process gs a -> IO a
runProcess gs proc = do
   let Process proc' = proc
   proc' gs

-- Random
instance MonadRandom (Process gs) where
   getRandom     = Process $ \gs -> getRandom
   getRandoms    = Process $ \gs -> getRandoms
   getRandomR  x = Process $ \gs -> getRandomR  x
   getRandomRs x = Process $ \gs -> getRandomRs x

-- Logging
msgProcess :: GameState gs => String -> Process gs ()
msgProcess msg = Process $ \_ -> putStrLn msg

-- Input
inputProcess :: GameState gs => Process gs String
inputProcess = Process $ \_ -> getLine

-- Combinators
may :: GameState gs => Process gs a -> Process gs (Maybe a)
may proc = do
   msgProcess $ "[y/n]"
   l <- inputProcess
   if l == "y"
      then liftM Just proc
      else return Nothing

-- vim: set expandtab:
