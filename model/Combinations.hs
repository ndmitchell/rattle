
module Combinations(originalSched, consSched, aggrSched) where

import Sequential
import Speculate
import Types
import Data.Functor.Identity
import Debug.Trace as Trace

{- rattle's original scheduling policy
   this scheduler is a combination of 2 schedulers.
   What to try first; then what to try in case of failure
-}
originalSched :: State -> IO State
originalSched t = do
  st <- speculateAllSched t
  case st of
    (State _ _ _ _ (Hazard h t2) _ _) -> if isNonRecoverable h
                                         then return st
                                         else case seqSched st{done=t2, prevRun=[]} of 
                                                (Identity st) -> return st
    x -> return x

{- Tries speculative
   If there is a failure it keeps going and we just re-execute the failed reads.
   when they are encountered in the required list

    Don't speculate things that have failed previously.

    Need to loop; because could fail multiple times
-}
consSched :: State -> IO State
consSched st1 = do
  st <- speculateAllSched st1
  f st
  where f st@(State _ _ _ _ (Hazard h t) _ _)
          | isRecoverable h = do
              st <- speculateAfterSched st{done=t}
              f st
          | isRestartable h =
              case seqSched st{done=t, prevRun=[]} of
                (Identity st) -> return st
          | otherwise = return st
        f st = return st


{- does this work?
-}
aggrSched :: State -> IO State
aggrSched st1 = do
  st <- speculateAllSched st1
  f st
  where f st@(State _ _ _ _ (Hazard h t) _ _)
          | isRecoverable h = do
              st <- speculateAllSched st{done=t}
              f st
          | isRestartable h =
              case seqSched st of
                (Identity st) -> return st
          | otherwise = return st
        f st = return st
