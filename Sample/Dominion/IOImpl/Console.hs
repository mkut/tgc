{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Sample.Dominion.IOImpl.Console where

import Sample.Dominion.Base

import TableGameCombinator.Core

import System.IO
import Control.Monad.Trans
import qualified Control.Monad.State.Lazy as S
import Data.List

instance DomDevice Dom

instance IDevice Dom String where
   ask = lift getLine
instance ODevice Dom String where
   tell x = lift $ do { putStr x; hFlush stdout }
instance ODevice Dom [String] where
   tell = tell . (++"]\n") . ("["++) . intercalate "/"

runProcess :: Dom () -> IO ()
runProcess proc = do
   S.evalStateT proc initialState

-- vim: set expandtab:
