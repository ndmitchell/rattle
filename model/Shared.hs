{-# LANGUAGE RecordWildCards #-}

module Shared(isSomethingDone, addTraces, somethingToRequire) where

import Types
import qualified Data.HashSet as Set
import Data.List

isSomethingDone :: [Cmd] -> T -> Bool
isSomethingDone cmds ct = any isDone cmds
  where isDone Cmd{..} = ((head start) `add` cost) <= ct  

addTraces :: (Set.HashSet String, Set.HashSet String) -> [(Set.HashSet String,Set.HashSet String)] -> (Set.HashSet String, Set.HashSet String)
addTraces = foldl' (\(r,w) (rs,ws) -> (Set.union r rs, Set.union w ws)) 

somethingToRequire :: State -> Maybe Cmd
somethingToRequire (State toRun _ running done@(Tree d _) _) = f toRun running d
  where f [] _ _ = Nothing
        f (t:ts) xs d | elem t xs = Nothing
                      | inTree t d = f ts xs d
                      | otherwise = Just t

