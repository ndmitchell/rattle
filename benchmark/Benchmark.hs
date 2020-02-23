{-# LANGUAGE RecordWildCards #-}

module Benchmark(main) where

import Benchmark.Type
import Benchmark.FSATrace as FSATrace
import System.Directory.Extra
import Development.Rattle
import Development.Shake.Command
import System.Environment
import System.Time.Extra
import System.IO.Extra
import Control.Monad


benchmarks =
    ["fsatrace" * FSATrace.benchmark
    ]
    where (*) = (,)

checkout i = cmd_ "git reset --hard" ["origin/master~" ++ show i]

timed :: String -> Int -> IO () -> IO ()
timed msg j act = do
    (t, _) <- duration act
    putStrLn $ msg ++ " " ++ show j ++ " = " ++ showDuration t

main :: IO ()
main = do
    xs <- getArgs
    forM_ xs $ \x -> do
        let Just Benchmark{..} = lookup x benchmarks
        withTempDir $ \dir -> do
            let commitList = reverse [0..commits]
            withCurrentDirectory dir $ do
                cmd_ "git clone" repo "."
                -- generate all the Rattle files
                forM_ commitList $ \i -> do
                    checkout i
                    clean
                    generate $ "rattle_" ++ show i ++ ".txt"
                -- for different levels of parallelism
                forM_ [1..4] $ \j -> do
                    -- first build with make
                    clean
                    forM_ commitList $ \i -> do
                        checkout i
                        timed "make" j $ cmd_ "make" ["-j" ++ show j] (EchoStdout False)
                    timed "make-nop" j $ cmd_ "make" ["-j" ++ show j] (EchoStdout False)

                    -- now with rattle
                    clean
                    removeDirectoryRecursive ".rattle"
                    let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[]}
                    forM_ commitList $ \i -> do
                        checkout i
                        cmds <- map words . lines <$> readFile' ("rattle_" ++ show i ++ ".txt")
                        timed "rattle" j $ rattleRun opts $ forM_ cmds cmd
                    cmds <- map words . lines <$> readFile' ("rattle_0.txt")
                    timed "rattle-nop" j $ rattleRun opts $ forM_ cmds cmd
