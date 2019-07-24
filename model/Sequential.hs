{-# LANGUAGE RecordWildCards, TupleSections #-}

module Sequential(seqSched) where

import Types
import Scheduler
import Shared
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Functor.Identity
import Debug.Trace as Trace

-- Decides whether to start a cmd or finish a cmd
seqOracle :: Int -> State -> Identity Action
seqOracle 1 (State tr pr r _ _ t _) | not $ Map.member 1 r = return Start
                                    | otherwise = if isSomethingDone 1 r t
                                                  then return Finished
                                                  else return Wait
seqOracle n st = return Wait

pickCmd :: Int -> State -> Cmd
pickCmd 1 (State toRun _ running _ (Tree d _) _ _)
  | not $ Map.member 1 running = f toRun d
  where f (t:ts) d | inTree t d = f ts d
                   | otherwise = t

-- continues to take a step until a hazard is encountered or the build is done; nothing running or to run
seqSched :: State -> Identity State
seqSched = sched seqOracle pickCmd
