{-# LANGUAGE RecordWildCards #-}

module Shared(isSomethingDone, addTraces) where

import Types
import qualified Data.HashSet as Set
import Data.List

isSomethingDone :: [Cmd] -> T -> Bool
isSomethingDone cmds ct = any isDone cmds
  where isDone Cmd{..} = ((head start) `add` cost) <= ct  

addTraces :: (Set.HashSet String, Set.HashSet String) -> [(Set.HashSet String,Set.HashSet String)] -> (Set.HashSet String, Set.HashSet String)
addTraces = foldl' (\(r,w) (rs,ws) -> (Set.union r rs, Set.union w ws)) 

