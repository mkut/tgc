import Sample.Dominion
import TableGameCombinator.Core

main = do
   is <- initialState
   runProcess is initialProcess
   return ()

-- vim: set expandtab:
