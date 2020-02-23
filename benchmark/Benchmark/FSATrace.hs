{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Benchmark.FSATrace(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra


main :: Args -> IO ()
main = vsMake VsMake{..}
    where
        broken = ["eafc609"]

        repo = "https://github.com/jacereda/fsatrace"

        generateVersion = 1

        generate :: IO String
        generate = do
            cmd_ "make clean" (EchoStdout False)
            Stdout xs <- cmd "make -j1"
            return $
                replace "fsatrace.exe" "fsatrace_.exe" $
                replace "-MMD" "" xs

        clean :: IO ()
        clean = cmd_ "make clean" (EchoStdout False)
