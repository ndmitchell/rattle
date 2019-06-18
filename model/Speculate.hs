{-# LANGUAGE RecordWildCards, TupleSections #-}

module Speculate(speculateSched) where

import Types
import Scheduler
import System.Random
import Data.Maybe
import Data.List
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

speculativeOracle :: State -> IO Action
speculativeOracle st@(State tr pr r _ t)
  | (isSomethingDone r t) && (somethingToRun st) = do
      r_ <- getStdRandom $ randomR ((0,1) :: (Integer,Integer))
      case r_ of
        0 -> return Finished
        1 -> return Start
  | isSomethingDone r t = return Finished
  | somethingToRun st = return Start
  | otherwise = return Wait
                   
somethingToRun :: State -> Bool
somethingToRun st = (isJust $ somethingToSpeculate st) || (isJust $ somethingToRequire st)

somethingToRequire :: State -> Maybe Cmd
somethingToRequire State{..} = f toRun running $ fst done
  where f [] _ _ = Nothing
        f (t:ts) xs d | elem t xs = Nothing
                      | inTree t d = f ts xs d
                      | otherwise = Just t
                       
somethingToSpeculate :: State -> Maybe Cmd
somethingToSpeculate State{..}
  | any (null . traces) running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) running)
                (filter (\c -> null $ traces c) prevRun) (snd done)
  -- helper :: (Set String, Set String) -> [Cmd] -> Map String ??
  where helper _ [] _ = Nothing
        helper rw (x:xs) h
          | elem x running || inTree x (fst done) = helper rw xs h
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

-- preference for a required cmd; dunno if this is equivalent to rattle
pickCmd :: State -> Cmd
pickCmd st = case somethingToRequire st of 
               Just c -> c
               Nothing -> fromJust $ somethingToSpeculate st

speculateSched :: State -> IO (Either State Hazard)
speculateSched = sched speculativeOracle pickCmd
          
