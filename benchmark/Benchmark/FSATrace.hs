{-# LANGUAGE RecordWildCards #-}

module Benchmark.FSATrace(benchmark) where

import Benchmark.Type
import Development.Shake.Command
import Data.List.Extra


benchmark :: Benchmark
benchmark = Benchmark{..}
    where
        repo = "https://github.com/jacereda/fsatrace"

        commits = 1

        generate :: FilePath -> IO ()
        generate file = do
            cmd_ "make clean"
            Stdout xs <- cmd "make -j1"
            writeFile file $
                replace "fsatrace.exe" "fsatrace_.exe" $
                replace "-MMD" "" xs

        clean :: IO ()
        clean = cmd_ "make clean" (EchoStdout False)
