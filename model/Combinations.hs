
module Combinations(originalSched, consSched) where

import Sequential
import Speculate
import Types
import Data.Functor.Identity

{- rattle's original scheduling policy
   this scheduler is a combination of 2 schedulers.
   What to try first; then what to try in case of failure
-}
originalSched :: State -> IO State
originalSched t = do
  st <- speculateAllSched t
  case st of
    (State _ _ _ (Hazard h _) _) -> case seqSched t of -- throw away failed speculatedrun
                                   (Identity st) -> return st
    x -> return x

{- Tries speculative
   If there is a failure it keeps going and we just re-execute the failed reads.
   when they are encountered in the required list

    Don't speculate things that have failed previously. 
-}
consSched :: State -> IO State
consSched st = do
  st <- speculateAllSched st
  case st of
    (State _ _ _ (Hazard (ReadWriteHazard _ _ _ Recoverable) t) _) ->
      speculateAfterSched st{done=t}
    x -> return x
