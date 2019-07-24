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
import Debug.Trace as Trace


-- trying to emulate rattle where we have more than 1 thread
-- which is speculating and running required cmds.
-- since everything has a complete order then we do one or the other
-- but required we run them in order waiting for them to complete
pickCmdAll :: Int -> State -> Cmd
pickCmdAll 1 st = fromJust $ somethingToRequire st
pickCmdAll n st = fromJust $ somethingToSpeculateAll n st

-- speculates with abandon
speculateAllOracle :: Int -> State -> IO Action
speculateAllOracle n st@(State tr pr r _ _ t _)
  | (isSomethingDone n r t) && (somethingToRunAll n st) = do
      r_ <- getStdRandom $ randomR ((0,1) :: (Integer,Integer))
      case r_ of
        0 -> return Finished
        1 -> return Start
  | isSomethingDone n r t = return Finished
  | somethingToRunAll n st = return Start
  | otherwise = return Wait
                   
somethingToRunAll :: Int -> State -> Bool
somethingToRunAll 1 st = isJust $ somethingToRequire st
somethingToRunAll n st = isJust $ somethingToSpeculateAll n st

somethingToSpeculateAll :: Int -> State -> Maybe Cmd
somethingToSpeculateAll n (State _ prevRun running _ (Tree d fs) _ l)
  | (n == 1) || (l < 2) || ((length $ Map.elems running) >= l) = Nothing
  | any (null . traces) $ Map.elems running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) $ Map.elems running)
                (filter (\c -> not $ null $ traces c) prevRun) fs
  -- helper :: (Set String, Set String) -> [Cmd] -> Map String ??
  where helper _ [] _ = Nothing
        helper rw (x:xs) h
          | (elem x $ Map.elems running) || inTree x d = helper rw xs h
        helper rw@(r,w) (x:xs) hazards
          | any (\y -> Set.member y w || Set.member y r || Map.member y hazards) $ Set.toList (foldl' (\ws (_,w) -> ws `Set.union` w) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs hazards -- still need to check done cmds for some reason?
        helper rw@(r,w) (x:xs) h -- if any of x's reads are members of w, then skip this one.
          | any (`Set.member` w) $ Set.toList (foldl' (\rs (r,_) -> rs `Set.union` r) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs h
        helper _ (x:xs) _ = Just x
somethingToSpeculateAll n st = Trace.trace ("state is: " ++ show st) Nothing

speculateAllSched :: State -> IO State
speculateAllSched = sched speculateAllOracle pickCmdAll

----------- don't speculate things that have failed --------------------------
pickCmd :: Int -> State -> Cmd
pickCmd 1 st = fromJust $ somethingToRequire st
pickCmd n st = fromJust $ somethingToSpeculate st

-- need a new oracle in the case of re-execution
speculativeOracle :: Int -> State -> IO Action
speculativeOracle n st@(State tr pr r _ _ t _)
  | (isSomethingDone n r t) && (somethingToRun n st) = do
      r_ <- getStdRandom $ randomR ((0,1) :: (Integer,Integer))
      case r_ of
        0 -> return Finished
        1 -> return Start
  | isSomethingDone n r t = return Finished
  | somethingToRun n st = return Start
  | otherwise = return Wait

somethingToRun :: Int -> State -> Bool
somethingToRun 1 st = isJust $ somethingToRequire st
somethingToRun n st = isJust $ somethingToSpeculate st

-- need new definition for something to speculate
somethingToSpeculate :: State -> Maybe Cmd
somethingToSpeculate (State _ prevRun running _ (Tree t fs) _ l)
  | (l < 2) || ((length $ Map.elems running) >= l) = Nothing
  | any (null . traces) $ Map.elems running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) $ Map.elems running)
                (filter (\c -> not $ null $ traces c) prevRun) fs
  -- helper :: (Set String, Set String) -> [Cmd] -> Map String ??
  where helper _ [] _ = Nothing
        helper rw (x:xs) h
          | (elem x $ Map.elems running) || inTree x t || inTreeFailed x t = helper rw xs h
        helper rw@(r,w) (x:xs) hazards
          | any (\y -> Set.member y w || Set.member y r || Map.member y hazards) $ Set.toList (foldl' (\ws (_,w) -> ws `Set.union` w) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs hazards -- still need to check done cmds for some reason?
        helper rw@(r,w) (x:xs) h -- if any of x's reads are members of w, then skip this one.
          | any (`Set.member` w) $ Set.toList (foldl' (\rs (r,_) -> rs `Set.union` r) Set.empty $ traces x)
          = helper (addTraces rw $ traces x) xs h
        helper _ (x:xs) _ = Just x

speculateAfterSched :: State -> IO State
speculateAfterSched = sched speculativeOracle pickCmd

          
