
module Benchmark.Type(
    Benchmark(..)
    ) where


data Benchmark = Benchmark
    {repo :: String
    ,commits :: Int
    ,generate :: FilePath -> IO ()
    ,clean :: IO ()
    }
