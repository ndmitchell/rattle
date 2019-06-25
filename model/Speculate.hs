{-# LANGUAGE RecordWildCards, TupleSections #-}

module Speculate(speculateAllSched, speculateAfterSched) where

import Types
import Scheduler
import Shared
import System.Random
import Data.Maybe
import Data.List
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

-- preference for a required cmd; dunno if this is equivalent to rattle
pickCmd :: State -> Cmd
pickCmd st = case somethingToRequire st of 
               Just c -> c
               Nothing -> fromJust $ somethingToSpeculate st

-- speculates with abandon
speculateAllOracle :: State -> IO Action
speculateAllOracle st@(State tr pr r _ t)
  | (isSomethingDone r t) && (somethingToRunAll st) = do
      r_ <- getStdRandom $ randomR ((0,1) :: (Integer,Integer))
      case r_ of
        0 -> return Finished
        1 -> return Start
  | isSomethingDone r t = return Finished
  | somethingToRunAll st = return Start
  | otherwise = return Wait
                   
somethingToRunAll :: State -> Bool
somethingToRunAll st = (isJust $ somethingToSpeculateAll st) || (isJust $ somethingToRequire st)

somethingToSpeculateAll :: State -> Maybe Cmd
somethingToSpeculateAll (State _ prevRun running (Tree d fs) _)
  | any (null . traces) running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) running)
                (filter (\c -> not $ null $ traces c) prevRun) fs
  -- helper :: (Set String, Set String) -> [Cmd] -> Map String ??
  where helper _ [] _ = Nothing
        helper rw (x:xs) h
          | elem x running || inTree x d = helper rw xs h
        helper rw@(r,w) (x:xs) hazards
          | any (\y -> Set.member y w || Set.member y r || Map.member y hazards) $ Set.toList (foldl' (\ws (_,w) -> ws `Set.union` w) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs hazards -- still need to check done cmds for some reason?
        helper rw@(r,w) (x:xs) h -- if any of x's reads are members of w, then skip this one.
          | any (`Set.member` w) $ Set.toList (foldl' (\rs (r,_) -> rs `Set.union` r) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs h
        helper _ (x:xs) _ = Just x

speculateAllSched :: State -> IO State
speculateAllSched = sched speculativeOracle pickCmd

----------- don't speculate things that have failed --------------------------

-- need a new oracle in the case of re-execution
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

-- need new definition for something to speculate
somethingToSpeculate :: State -> Maybe Cmd
somethingToSpeculate (State _ prevRun running (Tree t fs) _)
  | any (null . traces) running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) running)
                (filter (\c -> not $ null $ traces c) prevRun) fs
  -- helper :: (Set String, Set String) -> [Cmd] -> Map String ??
  where helper _ [] _ = Nothing
        helper rw (x:xs) h
          | elem x running || inTree x t || inTreeFailed x t = helper rw xs h
        helper rw@(r,w) (x:xs) hazards
          | any (\y -> Set.member y w || Set.member y r || Map.member y hazards) $ Set.toList (foldl' (\ws (_,w) -> ws `Set.union` w) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs hazards -- still need to check done cmds for some reason?
        helper rw@(r,w) (x:xs) h -- if any of x's reads are members of w, then skip this one.
          | any (`Set.member` w) $ Set.toList (foldl' (\rs (r,_) -> rs `Set.union` r) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs h
        helper _ (x:xs) _ = Just x

speculateAfterSched :: State -> IO State
speculateAfterSched = sched speculativeOracle pickCmd

          
