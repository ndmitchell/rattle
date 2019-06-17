{-# LANGUAGE RecordWildCards #-}

module Shared(isSomethingDone, getFinished) where

import Types

isSomethingDone :: [Cmd] -> T -> Bool
isSomethingDone cmds ct = any isDone cmds
  where isDone Cmd{..} = ((head start) `add` cost) <= ct  

getFinished :: [Cmd] -> T -> Cmd
getFinished xs t = getEarliest $ filter isDone xs
  where isDone Cmd{..} = ((head start) `add` cost) <= t
        getEarliest (x:xs) = foldl (\e x -> if ((head $ start e) `add` cost e) < ((head $ start x) `add` cost x)
                                            then e
                                            else x) x xs
