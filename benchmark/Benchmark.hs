{-# LANGUAGE RecordWildCards #-}

module Benchmark(main) where

import Benchmark.Args
import qualified Benchmark.FSATrace
import qualified Benchmark.Micro
import Control.Monad


benchmarks =
    ["fsatrace" * Benchmark.FSATrace.main
    ,"micro" * Benchmark.Micro.main
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
