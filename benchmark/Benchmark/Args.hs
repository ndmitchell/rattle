{-# LANGUAGE DeriveDataTypeable #-}

module Benchmark.Args(
    Args(..), getArguments,
    elemOrNull
    ) where

import System.Console.CmdArgs

data Args = Args
    {names :: [String] -- the benchmarks to run
    ,threads :: [Int] -- which thread numbers to use
    ,repeat_ :: Maybe Int
    ,step :: [String] -- which steps to run
    }
    deriving Data


getArguments :: IO Args
getArguments = cmdArgs $ Args
    {names = [] &= args
    ,threads = [] &= name "j"
    ,repeat_ = Nothing
    ,step = []
    }


elemOrNull :: Eq a => a -> [a] -> Bool
elemOrNull x xs = null xs || x `elem` xs
