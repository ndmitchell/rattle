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

step :: Monad m => (State -> m Action) -> (State -> Cmd) -> State -> m (Either State Hazard)
step f f2 st = do
  a <- oracle f st
  case a of
    Finished -> finish st
    Start    -> return (Left $ run f2 st)
    _        -> return $ Left st{timer=(succ $ timer st)}


{- Is finish the same for all schedulers? -}
finish :: Monad m => State -> m (Either State Hazard)
finish st@State{..} = let e = getFinished running timer
                          ne = e{stop=timer:(stop e), traces=(rfiles e, wfiles e):(traces e)}
                          nh = Map.fromList $ map (,(Write,head (stop ne) ,e)) (wfiles e) ++
                                              map (,(Read ,head (start ne),e)) (rfiles e) in
                        case (Tree (fst done) (snd done)) <> Tree (L e) nh of
                          Hazard h -> return $ Right h
                          Tree t f -> return $ Left st{running=(delete e running)
                                                      ,done=(t,f)
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

sched :: Monad m => (State -> m Action) -> (State -> Cmd) -> State -> m (Either State Hazard)
sched o p st = f $ Left st
  where f (Right h) = return $ Right h
        f (Left st@(State r _ _ d _)) | inTree (last r) (fst d) = return $ Left st
                                      | otherwise = do
                                          nst <- step o p st
                                          f nst


