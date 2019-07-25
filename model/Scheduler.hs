{-# LANGUAGE RecordWildCards, TupleSections #-}

module Scheduler(sched) where

import Types
import Shared
import qualified Data.HashMap.Strict as Map
import Data.List
import qualified Data.HashSet as Set

{- An oracle which decides what step to take next [in the case of choice] -}
oracle :: Monad m => Int -> (Int -> State -> m Action) -> State -> m Action
oracle n f st@(State tr pr r rq _ _ _) | Map.null r && isDone st = return Done
                                       | otherwise = f n st -- choice

-- the torun list hasn't been completed
isDone :: State -> Bool
isDone (State tr _ _ _ (Tree t _) _ _) = all (`inTree` t) tr
isDone (State tr _ _ _ (Hazard h (Tree t _)) _ _) = all (`inTree` t) tr

{- Deciding whether there is something to run -}

{- Taking a step -}

stepN :: Monad m => Int -> (Int -> State -> m Action) -> (Int -> State -> Cmd) -> State -> m State
stepN 0 f f2 st = return st
stepN n f f2 st@(State _ _ _ _ (Hazard _ _) _ _) = return st
stepN n f f2 st = do
  st <- step n f f2 st
  stepN (n-1) f f2 st

{- Required list:
   Is required running something? If yes then don't change
   Is required doing nothing? if yes then maybe change
   How do we know whether to change? The latest thing in torun list that is
   currently being run by another thread.

-}

step :: Monad m => Int -> (Int -> State -> m Action) -> (Int -> State -> Cmd) -> State -> m State
step n f f2 st = do
  a <- oracle n f st
  case a of
    Finished -> finish n st
    Start    -> return $ run n f2 st
    _        -> if n == 1
                then if Map.member 1 $ running st
                     then return st{timer=succ $ timer st} -- don't change required index
                     else return st{required=f st
                                   ,timer=succ $ timer st}
                else return st{timer=succ $ timer st}
      where f State{..} = g required (Map.elems running) toRun
            g i [] ls = i
            g i (x:xs) ls = case elemIndex x ls of
                              (Just i2) -> g (max i2 i) xs ls
                              Nothing -> g i xs ls


{- Is finish the same for all schedulers? -}
finish :: Monad m => Int -> State -> m State
finish n st@(State tr pr running rq t@(Tree t2 _) timer _) =
  let (Just e) = Map.lookup n running
      ne = e{stop=timer, traces=(rfiles e, wfiles e):traces e}
      nh = Map.fromList $ map (,[(Write,stop ne,ne)]) (Set.toList $ wfiles e) ++
           map (,[(Read ,start ne,ne)]) (Set.toList (rfiles e `Set.difference` wfiles e)) in
    return $ st{running=Map.delete n running
               ,done=merge (take (rq+1) tr)
                       t $ Tree (L ne No) nh
               ,timer=succ timer}

{- Something to run a cmd; how to pick cmd to run -}
run :: Int -> (Int -> State -> Cmd) -> State -> State
run 1 pick st@State{..} = let e = pick 1 st
                              (Just i) = elemIndex e toRun in
                            st{running=Map.insert 1 e{start=timer} running
                              ,required=i
                              ,timer=succ timer}
run n pick st@State{..} = let e = pick n st in
                            st{running=Map.insert n e{start=timer} running
                              ,timer=succ timer}

{- scheduler function that runs to completion -}
sched :: Monad m => (Int -> State -> m Action) -> (Int -> State -> Cmd) -> State -> m State
sched o p = f
  where f st@(State _ _ _ _ (Hazard h _) _  _) = return st
        f st@(State r _ _ _ (Tree t hs) _ l) | all (`inTree` t) r = return $ update st
                                           | otherwise = do
                                               nst <- stepN l o p st
                                               f nst

update :: State -> State
update st@(State toRun _ _ _ (Tree t hs) _ _) = st{prevRun=map (\c@Cmd{..} -> c{pos=(fst pos, Speculated),traces=getTraces c}) toRun}
  where getTraces c = let (Just c2) = getCmd c t in
                        traces c2
