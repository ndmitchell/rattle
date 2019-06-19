{-# LANGUAGE RecordWildCards, TupleSections #-}

module Scheduler(sched, isSomethingDone) where

-- How do we define a scheduler?
import Types
import qualified Data.HashMap.Strict as Map
import Data.List

{- An oracle which decides what step to take next [in the case of choice] -}
oracle :: Monad m => (State -> m Action) -> State -> m Action
oracle _ (State [] pr [] _ _) = return Done
oracle _ (State tr pr [] _ _) = return Start
oracle _ (State [] pr r _ t) = if isSomethingDone r t
                               then return Finished
                               else return Wait
oracle f st = f st -- choice

isSomethingDone :: [Cmd] -> T -> Bool
isSomethingDone cmds ct = any isDone cmds
  where isDone Cmd{..} = ((head start) `add` cost) <= ct  

{- Deciding whether there is something to run -}

{- Taking a step -}

step :: Monad m => (State -> m Action) -> (State -> Cmd) -> State -> m State
step f f2 st = do
  a <- oracle f st
  case a of
    Finished -> finish st
    Start    -> return $ run f2 st
    _        -> return st{timer=(succ $ timer st)}


{- Is finish the same for all schedulers? -}
finish :: Monad m => State -> m State
finish st@(State _ pr running (Left done) timer) =
  let e = getFinished running timer
      ne = e{stop=timer:(stop e), traces=(rfiles e, wfiles e):(traces e)}
      nh = Map.fromList $ map (,(Write,head (stop ne) ,e)) (wfiles e) ++
           map (,(Read ,head (start ne),e)) (rfiles e) in
    case (Tree (fst done) (snd done)) <> Tree (L e) nh of
      Hazard h -> return $ st{running=(delete e running)
                             ,done=(Right h)
                             ,timer=(succ timer)}
      Tree t f -> return $ st{running=(delete e running)
                             ,done=(Left (t,f))
                             ,timer=(succ timer)}

getFinished :: [Cmd] -> T -> Cmd
getFinished xs t = getEarliest $ filter isDone xs
  where isDone Cmd{..} = ((head start) `add` cost) <= t
        getEarliest (x:xs) = foldl (\e x -> if ((head $ start e) `add` cost e) < ((head $ start x) `add` cost x)
                                            then e
                                            else x) x xs

{- Something to run a cmd; how to pick cmd to run -}

run :: (State -> Cmd) -> State -> State
run f st@State{..} = let e = f st in
                       st{running=(e{start=timer:(start e)}:running), timer=(succ timer)}

{- scheduler function that runs to completion -}

sched :: Monad m => (State -> m Action) -> (State -> Cmd) -> State -> m State
sched o p st = f $ resetState st
  where f st@(State _ _ _ (Right h) _) = return st 
        f st@(State r _ _ (Left d) _) | inTree (last r) (fst d) = return $ update st
                               | otherwise = do
                                   nst <- step o p st
                                   f nst

update :: State -> State
update st@State{..} = st{prevRun=(map (\c@Cmd{..} -> c{pos=(fst pos, Speculated)}) toRun)} 


