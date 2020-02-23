{-# LANGUAGE RecordWildCards #-}

module Benchmark(main) where

import Benchmark.Args
import Benchmark.FSATrace as FSATrace
import Control.Monad


benchmarks =
    ["fsatrace" * FSATrace.benchmark
    ]
    where (*) = (,)


main :: IO ()
main = do
    args@Args{..} <- getArguments
    when (null names) $
        error $ "Specify which benchmarks to run, from: " ++ show (map fst benchmarks)
    forM_ names $ \name -> do
        let Just m = lookup name benchmarks
        m args
