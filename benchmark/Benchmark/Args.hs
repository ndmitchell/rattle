
module Benchmark.Args(Args(..), getArguments) where

import System.Console.CmdArgs

data Args = Args
    {names :: [String] -- the benchmarks to run
    ,threads :: [Int] -- which thread numbers to use
    ,repeats :: Maybe Int
    ,steps :: [String] -- which steps to run
    }


getArguments :: IO Args
getArguments = undefined
