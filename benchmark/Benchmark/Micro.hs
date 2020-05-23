{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Benchmark.Micro(main) where

import Benchmark.Args
import System.Process
import System.IO.Extra
import Control.Monad
import Data.Maybe
import Data.List.Extra
import System.Time.Extra
import Numeric.Extra
import Development.Shake hiding (readFile', withTempDir)
import qualified Data.ByteString as BS
import Development.Rattle

main :: Args -> IO ()
main Args{..} = do
    let clean = "make clean"
    let run = "commands.txt"
    cmds <- map words . lines <$> readFile' run

    let benchmark lbl act = when (lbl `elemOrNull` step) $ do
            putStr $ lbl ++ ": "
            hFlush stdout
            let count = fromMaybe 5 repeat_
            -- ignore the slowest few times when taking the average
            let ignore = if count >= 5 then 2 else 0
            times <- replicateM count $ withTempDir $ \dir -> do
                cmd_ Shell clean (EchoStdout False)
                fst <$> duration (act dir)
            putStrLn $ unwords (map showDuration times) ++ " = " ++ showDuration (sum (dropEnd ignore $ sort times) / intToDouble (count-ignore))

    benchmark "make" $ const $
        cmd_ "make -j1" (EchoStdout False)

    benchmark "System.Process" $ const $
        forM_ cmds $ \(command:args) ->
            callProcess command args

    benchmark "shake.cmd" $ const $
        forM_ cmds cmd_

    benchmark "shake.cmd fsatrace" $ const $
        forM_ cmds $ \xs -> cmd_ $ "fsatrace" : "rwmdqt" : "fsatrace.out" : "--" : xs

    benchmark "shake.cmd traced" $ const $
        forM_ cmds $ \xs -> do
            _ :: [FSATrace BS.ByteString] <- cmd xs
            pure ()

    let opts dir = rattleOptions{rattleFiles=dir, rattleProcesses=1, rattleUI=Just RattleQuiet, rattleNamedDirs=[]}
    benchmark "rattle" $ \dir ->
        rattleRun (opts dir){rattleSpeculate=Nothing, rattleShare=False} $
            forM_ cmds cmd

    benchmark "rattle share" $ \dir ->
        rattleRun (opts dir) $
            forM_ cmds cmd
