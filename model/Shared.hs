{-# LANGUAGE RecordWildCards #-}

module Shared(isSomethingDone, addTraces, somethingToRequire) where

import Types
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List

isSomethingDone :: Int -> Map.HashMap Int Cmd -> T -> Bool
isSomethingDone n cmds ct | Map.member n cmds =
                              let Cmd{..} = cmds Map.! n in
                                (start `add` cost) <= ct
                          | otherwise = False

addTraces :: (Set.HashSet String, Set.HashSet String) -> [(Set.HashSet String,Set.HashSet String)] -> (Set.HashSet String, Set.HashSet String)
addTraces = foldl' (\(r,w) (rs,ws) -> (Set.union r rs, Set.union w ws)) 

somethingToRequire :: State -> Maybe Cmd
somethingToRequire (State toRun _ running rq (Tree d _) _ _)
  | Map.member 1 running = Nothing
  | otherwise = f toRun (Map.elems running) d
  where f [] _ _ = Nothing
        f (t:ts) xs d | t `elem` xs = Nothing
                      | inTree t d = f ts xs d
                      | otherwise = Just t
