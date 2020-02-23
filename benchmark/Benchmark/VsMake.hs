{-# LANGUAGE RecordWildCards #-}

module Benchmark.VsMake(
    VsMake(..), vsMake, Args,
    ) where

import Benchmark.Args
import System.Directory.Extra
import Development.Rattle
import Development.Shake.Command
import System.Time.Extra
import Data.Maybe
import System.IO.Extra
import Control.Monad.Extra


data VsMake = VsMake
    {repo :: String
    ,generate :: FilePath -> IO ()
    ,clean :: IO ()
    }


checkout i = cmd_ "git reset --hard" ["origin/master~" ++ show i]

timed :: String -> Int -> IO () -> IO ()
timed msg j act = do
    (t, _) <- duration act
    putStrLn $ msg ++ " " ++ show j ++ " = " ++ showDuration t

vsMake :: VsMake -> Args -> IO ()
vsMake VsMake{..} Args{..} = withTempDir $ \dir -> do
    let commitList = reverse [0..fromMaybe 10 commits]
    withCurrentDirectory dir $ do
        cmd_ "git clone" repo "."
        -- generate all the Rattle files
        forM_ commitList $ \i -> do
            checkout i
            generate $ "rattle_" ++ show i ++ ".txt"
        -- for different levels of parallelism
        forM_ (threads `orNull` [1..4]) $ \j -> do
            -- first build with make
            clean
            forM_ commitList $ \i -> do
                checkout i
                timed "make" j $ cmd_ "make" ["-j" ++ show j] (EchoStdout False)
            timed "make-nop" j $ cmd_ "make" ["-j" ++ show j] (EchoStdout False)

            -- now with rattle
            clean
            whenM (doesDirectoryExist ".rattle") $
                removeDirectoryRecursive ".rattle"
            let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[]}
            forM_ commitList $ \i -> do
                checkout i
                cmds <- map words . lines <$> readFile' ("rattle_" ++ show i ++ ".txt")
                timed "rattle" j $ rattleRun opts $ forM_ cmds cmd
            cmds <- map words . lines <$> readFile' ("rattle_0.txt")
            timed "rattle-nop" j $ rattleRun opts $ forM_ cmds cmd
