{-# LANGUAGE RecordWildCards, TupleSections #-}

module Sequential(seqsched) where

import Types
import Tree
import Shared
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List

-- Decides whether to start a cmd or finish a cmd
seqOracle :: State -> Action
seqOracle (State [] _ [] _ _) = Done
seqOracle (State tr pr [] _ _) = Start
seqOracle (State tr pr r _ t) = if isSomethingDone r t
                                then Finished 
                                else Wait

-- takes a step which costs 1 unit of time
seqStep :: State -> (Either State Hazard)
seqStep st = case seqOracle st of
               Finished -> finish st
               Start    -> Left $ seqrun st
               _        -> Left st{timer=(succ $ timer st)}

-- removes a cmd which finished earliest from running list and adds it to done list
-- then checks for hazards
finish :: State -> (Either State Hazard)
finish st@State{..} = let e = getFinished running timer 
                          ts = timer
                          tr = (rfiles e, wfiles e)
                          ne = e{stop=ts:(stop e),traces=tr:(traces e)} 
                          nh = Map.fromList $ map (,(Write,head (stop ne) ,e)) (wfiles e) ++
                                              map (,(Read ,head (start ne),e)) (rfiles e) in
                        case Tree.insert (Tree (fst done) (snd done)) $ Tree (L e) nh of
                          Hazard h -> Right h
                          Tree t f -> Left st{running=(delete e running)
                                           ,done=(t,f)
                                           ,timer=(succ timer)}

-- runs a cmd; takes first command from torun list and moves it to running
seqrun :: State -> State
seqrun st@State{..} = let e = head toRun
                          ts = timer in
                     st{toRun=(tail toRun), running=(e{start=ts:(start e)}:running), timer=(succ timer)}

-- continues to take a step until a hazard is encountered or the build is done; nothing running or to run
seqsched :: (Either State Hazard) -> (Either State Hazard)
seqsched (Right h) = Right h                          
seqsched (Left st@(State [] _ [] _ _)) = Left st
seqsched (Left st) = seqsched $ seqStep st

