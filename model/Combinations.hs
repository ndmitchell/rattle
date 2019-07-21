
module Combinations(originalSched, consSched, aggrSched) where

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

    Need to loop; because could fail multiple times
-}
consSched :: State -> IO State
consSched st = do
  st <- speculateAllSched st
  f st
  where f st@(State _ _ _ (Hazard h t) _)
          | isRecoverable h = do
              st <- speculateAfterSched st{done=t}
              f st
          | isRestartable h =
              case seqSched st of
                (Identity st) -> return st
          | otherwise = return st
        f st = return st


{- does this work?
-}
aggrSched :: State -> IO State
aggrSched st = do
  st <- speculateAllSched st
  f st
  where f st@(State _ _ _ (Hazard h t) _)
          | isRecoverable h = do
              st <- speculateAllSched st{done=t}
              f st
          | isRestartable h =
              case seqSched st of
                (Identity st) -> return st
          | otherwise = return st
        f st = return st
