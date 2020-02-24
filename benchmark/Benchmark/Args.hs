{-# LANGUAGE DeriveDataTypeable #-}

module Benchmark.Args(
    Args(..), getArguments,
    elemOrNull, orNull,
    ) where

import System.Console.CmdArgs

data Args = Args
    {names :: [String] -- the benchmarks to run
    ,threads :: [Int] -- which thread numbers to use
    ,repeat_ :: Maybe Int
    ,step :: [String] -- which steps to run
    ,commits :: Maybe Int
    ,no_stderr :: Bool
    ,count :: Maybe Int
    }
    deriving Data

mode :: Mode (CmdArgs Args)
mode = cmdArgsMode $ Args
    {names = [] &= args
    ,threads = [] &= name "j"
    ,repeat_ = Nothing
    ,step = []
    ,commits = Nothing
    ,no_stderr = False
    ,count = Nothing
    }

getArguments :: IO Args
getArguments = cmdArgsRun mode


elemOrNull :: Eq a => a -> [a] -> Bool
elemOrNull x xs = null xs || x `elem` xs

orNull :: [a] -> [a] -> [a]
orNull xs ys = if null xs then ys else xs
