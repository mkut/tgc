import qualified Sample.Dominion as D
import Sample.Dominion.IOImpl.Console (runProcess)

main = do
   runProcess D.main
   return ()

-- vim: set expandtab:
