
module Conservative(consSched) where

import Types
import Shared
import System.Random
import Data.Maybe
import Data.List
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Speculate
import Scheduler
{- Tries speculative
   If there is a failure it keeps going and we just re-execute the failed reads.
   when they are encountered in the required list

    Don't speculate things that have failed previously. 
-}
consSched :: State -> IO State
consSched st = do
  st <- speculateSched st
  case st of
    (State _ _ _ (Hazard (ReadWriteHazard _ _ _ Recoverable) _) _) ->
      speculateAfterSched st
    x -> return x

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

somethingToRequire :: State -> Maybe Cmd
somethingToRequire (State toRun _ running (Tree t fs) _) = f toRun running t
  where f [] _ _ = Nothing
        f (t:ts) xs d | elem t xs = Nothing
                      | inTree t d = f ts xs d
                      | otherwise = Just t

-- need new definition for something to speculate
somethingToSpeculate :: State -> Maybe Cmd
somethingToSpeculate (State _ prevRun running (Tree t fs) _)
  | any (null . traces) running = Nothing
  | otherwise = helper (foldl' (\p r -> addTraces p $ traces r) (Set.empty, Set.empty) running)
                (filter (\c -> null $ traces c) prevRun) fs
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

-- preference for a required cmd; dunno if this is equivalent to rattle
pickCmd :: State -> Cmd
pickCmd st = case somethingToRequire st of 
               Just c -> c
               Nothing -> fromJust $ somethingToSpeculate st

speculateAfterSched :: State -> IO State
speculateAfterSched = sched speculativeOracle pickCmd
