import qualified Sample.Dominion as D
import TableGameCombinator.Core

main = do
   runProcess D.main D.initialState
   return ()

-- vim: set expandtab:
