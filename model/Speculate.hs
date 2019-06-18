{-# LANGUAGE RecordWildCards, TupleSections #-}

module Speculate where

import Types
import Shared
import System.Random
import Data.Maybe
import Data.List
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

speculativeOracle :: State -> IO Action
speculativeOracle (State [] pr [] _ _) = return Done
speculativeOracle (State tr pr [] _ _) = return Start
speculativeOracle (State [] pr r _ t) = if isSomethingDone r t
                                     then return Finished
                                     else return Wait
speculativeOracle st@(State tr pr r _ t) = do
  r_ <- getStdRandom $ randomR ((0,1) :: (Integer,Integer))
  case r_ of
    0 -> if isSomethingDone r t
         then return Finished
         else if somethingToRun st
              then return Start
              else return Wait
    1 -> if somethingToRun st
         then return Start
         else if isSomethingDone r t
              then return Finished
              else return Wait
somethingToRun :: State -> Bool
somethingToRun st = (isJust $ somethingToSpeculate st) || (isJust $ somethingToRequire st)

{- if everything before the first cmd in torun is "done"; aka isn't in running list
   then can run the first thing in toRun?
-}
somethingToRequire :: State -> Maybe Cmd
somethingToRequire State{..} = let (T i) = pos $ head toRun in
                                 helper i running
  where helper 0 rn = Just $ head toRun
        helper x rn = if any (\Cmd{..} -> let (T j) = pos in
                                            j == (x - 1)) rn
                      then Nothing
                      else helper (x - 1) rn

somethingToSpeculate :: State -> Maybe Cmd
somethingToSpeculate State{..}
  | any (null . traces) running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) running)
                (filter (\c -> null $ traces c) prevRun) (snd done)
  -- 2nd argument is the list of things we could speculate.
  -- things in the speculate list; so things we have data on.
  -- first arg is pair of lists of reads and writes
  where helper _ [] _ = Nothing  -- if any of x's writes are members of r or w, or the set of reads/writes of the done cmds; then skip this one
        helper rw@(r,w) (x:xs) hazards
          | any (\y -> Set.member y w || Set.member y r || Map.member y hazards) (foldl' (\ws (_,w) -> ws ++ w) [] $ traces x)
          = helper (addTraces rw $ traces x) xs hazards -- still need to check done cmds for some reason?
        helper rw@(r,w) (x:xs) h -- if any of x's reads are members of w, then skip this one.
          | any (`Set.member` w) (foldl' (\rs (r,_) -> rs ++ r) [] $ traces x)
          = helper (addTraces rw $ traces x) xs h
        helper _ (x:xs) _ = Just x

addTraces :: (Set.HashSet String, Set.HashSet String) -> [([String],[String])] -> (Set.HashSet String, Set.HashSet String)
addTraces (r,w) [] = (r,w)
addTraces (r,w) ((rs,ws):xs) = addTraces (Set.union r $ Set.fromList rs, Set.union w $ Set.fromList ws) xs

step :: State -> IO (Either State Hazard) --Monad m => State -> m (Either State Hazard)
step st = do
  a <- speculativeOracle st
  case a of
    Finished -> return $ finish st
    Start    -> return (Left $ run st)
    _        -> return $ Left st{timer=(succ $ timer st)}

-- need to populate cmds trace info after cmd finishes.
finish :: State -> (Either State Hazard)
finish st@State{..} = let e = getFinished running timer
                          ts = timer
                          tr = (rfiles e, wfiles e)
                          ne = e{stop=ts:(stop e), traces=tr:(traces e)}
                          nh = Map.fromList $ map (,(Write,head (stop ne) ,e)) (wfiles e) ++
                                              map (,(Read ,head (start ne),e)) (rfiles e) in
                        case (Tree (fst done) (snd done)) <> Tree (L e) nh of
                          Hazard h -> Right h
                          Tree t f -> Left st{running=(delete e running)
                                             ,done=(t,f)
                                             ,timer=(succ timer)}

run :: State -> State
run st@State{..} = let e = pickCmd st
                       ts = timer in
                     st{toRun=(delete e toRun), running=(e{start=ts:(start e)}:running), timer=(succ timer)}

-- preference for a required cmd; dunno if this is equivalent to rattle
pickCmd :: State -> Cmd
pickCmd st = case somethingToRequire st of 
               Just c -> c
               Nothing -> fromJust $ somethingToSpeculate st

speculateSched :: State -> IO (Either State Hazard)
speculateSched t = f $ Left t
  where f (Right h) = return $ Right h
        f (Left st@(State [] _ [] _ _)) = return $ Left st
        f (Left st) = do 
          nst <- step st
          speculateSched nst
          
