{-# LANGUAGE RankNTypes #-}
module TableGameCombinator.Core
   ( Process

   , runProcess

   , tell
   , line
   , module Control.Monad.Trans.State.Lazy

   , may
   , mayS

   , choose
   ) where

import System.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.ByteString (ByteString)

-- Game Monad
type Process gs = StateT gs IO

-- Execution
runProcess :: Process gs a -> gs -> IO a
runProcess proc gs = evalStateT proc gs

-- Lifted IO functions
tell :: String -> Process gs ()
tell x = lift $ do { putStr x; hFlush stdout}

line :: Process gs String
line = lift getLine

-- Combinators
may :: Process gs a -> Process gs (Maybe a)
may = mayS ""

mayS :: String -> Process gs a -> Process gs (Maybe a)
mayS str proc = do
   tell $ if str == "" then "" else str ++ " [y/n]"
   l <- line
   case l of
      "y" -> liftM Just proc
      "n" -> return Nothing
      _ -> do
         tell $ "Type 'y' or 'n'.\n"
         mayS str proc

choose :: [(String, Process gs a)] -> Process gs a
choose alts = do
   l <- line
   case lookup l alts of
      Just proc -> proc
      Nothing   -> choose alts

-- vim: set expandtab:
