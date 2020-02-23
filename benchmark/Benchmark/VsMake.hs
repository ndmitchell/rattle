{-# LANGUAGE RecordWildCards #-}

module Benchmark.VsMake(
    VsMake(..), vsMake, Args,
    ) where

import Benchmark.Args
import System.Directory.Extra
import Development.Rattle
import Development.Shake.Command
import System.Time.Extra
import Control.Exception
import System.FilePath
import Data.Maybe
import System.IO.Extra
import Control.Monad.Extra


data VsMake = VsMake
    {repo :: String
    ,generate :: IO String
    ,generateVersion :: Int
    ,clean :: IO ()
    ,broken :: [String]
    }


gitCheckout :: Int -> IO String
gitCheckout i = do
    Stdout x <- cmd "git reset --hard" ["origin/master~" ++ show i]
    -- HEAD is now at 41fbba1 Warning
    return $ words x !! 4


generateName :: VsMake -> String -> IO FilePath
generateName VsMake{..} commit = do
    tdir <- getTemporaryDirectory
    return $ tdir </> takeBaseName repo ++ "." ++ commit ++ "." ++ show generateVersion ++ ".txt"


timed :: String -> Int -> IO () -> IO ()
timed msg j act = do
    (t, _) <- duration act
    putStrLn $ msg ++ " " ++ show j ++ " = " ++ showDuration t

vsMake :: VsMake -> Args -> IO ()
vsMake vs@VsMake{..} Args{..} = withTempDir $ \dir -> do
    let commitList = reverse [0..fromMaybe 10 commits]

    let checkout i act = do
            commit <- gitCheckout i
            when (commit `notElem` broken) $
                flip onException (putStrLn $ "AT COMMIT " ++ commit) $
                    act commit

    withCurrentDirectory dir $ do
        cmd_ "git clone" repo "."

        -- generate all the Rattle files
        when ("generate" `elemOrNull` step) $ do
            putStrLn "GENERATING RATTLE SCRIPTS"
            forM_ commitList $ \i -> do
                putChar '.' >> hFlush stdout
                checkout i $ \commit -> do
                    file <- generateName vs commit
                    unlessM (doesFileExist file) $ do
                        res <- generate
                        evaluate $ length res
                        writeFile file res
            putStrLn ""

        -- for different levels of parallelism
        forM_ (threads `orNull` [1..4]) $ \j -> do
            -- first build with make
            when ("make" `elemOrNull` step) $ do
                putStrLn "BUILDING WITH MAKE"
                clean
                forM_ (commitList++[0]) $ \i -> do
                    checkout i $ \_ ->
                        timed "make" j $ cmd_ "make" ["-j" ++ show j] (EchoStdout False)

            -- now with rattle
            when ("rattle" `elemOrNull` step) $ do
                putStrLn "BUILDING WITH RATTLE"
                clean
                whenM (doesDirectoryExist ".rattle") $
                    removeDirectoryRecursive ".rattle"
                forM_ (commitList ++ [0]) $ \i -> do
                    checkout i $ \commit -> do
                        file <- generateName vs commit
                        cmds <- map words . lines <$> readFile' file
                        let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[]}
                        timed "rattle" j $ rattleRun opts $ forM_ cmds cmd