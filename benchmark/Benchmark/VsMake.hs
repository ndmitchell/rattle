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
import Data.List
import Data.IORef
import Data.Maybe
import System.IO.Extra
import Control.Monad.Extra


data VsMake = VsMake
    {repo :: String
    ,generate :: IO String
    ,generateVersion :: Int
    ,clean :: IO ()
    ,broken :: [String]
    ,make :: CmdArgument
    ,rattle :: CmdArgument
    ,master :: String
    }


gitCheckout :: VsMake -> Int -> IO String
gitCheckout VsMake{..} i = do
    Stdout x <- cmd "git reset --hard" ["origin/" ++ master ++ "~" ++ show i]
    -- HEAD is now at 41fbba1 Warning
    return $ words x !! 4


generateName :: VsMake -> String -> IO FilePath
generateName VsMake{..} commit = do
    tdir <- getTemporaryDirectory
    return $ tdir </> takeBaseName repo ++ "." ++ commit ++ "." ++ show generateVersion ++ ".txt"


timed :: IORef Seconds -> String -> Int -> Int -> IO () -> IO ()
timed ref msg j commit act = do
    (t, _) <- duration act
    appendFile "vsmake.log" $ intercalate "\t" [msg, show j, show commit, show t] ++ "\n"
    putStrLn $ msg ++ " " ++ show j ++ " (" ++ show commit ++ ") = " ++ showDuration t
    modifyIORef' ref (+ t)

vsMake :: VsMake -> Args -> IO ()
vsMake vs@VsMake{..} Args{..} = withTempDir $ \dir -> do
    let counted = maybe id take count
    let commitList = reverse [0..fromMaybe 10 commits]

    let checkout i act = do
            commit <- gitCheckout vs i
            when (commit `notElem` broken) $
                flip onException (putStrLn $ "AT COMMIT " ++ commit) $
                    act commit
    let stderr = [EchoStderr False | no_stderr]

    withCurrentDirectory dir $ do
        -- don't pass a depth argument, or git changes the length of the shown commit hashes
        -- which messes up caching and broken hashes
        cmd_ "git clone" repo "."

        -- generate all the Rattle files
        when ("generate" `elemOrNull` step) $ do
            putStrLn "GENERATING RATTLE SCRIPTS"
            forM_ (counted commitList) $ \i -> do
                putChar '.' >> hFlush stdout
                checkout i $ \commit -> do
                    file <- generateName vs commit
                    unlessM (doesFileExist file) $ do
                        res <- generate
                        evaluate $ length res
                        writeFile file res
            putStrLn ""


        -- for different levels of parallelism
        replicateM_ (fromMaybe 1 repeat_) $
            forM_ (threads `orNull` [1..4]) $ \j -> do
                makeTime <- newIORef 0
                rattleTime <- newIORef 0

                -- first build with make
                when ("make" `elemOrNull` step) $ do
                    putStrLn "BUILDING WITH MAKE"
                    clean
                    forM_ (counted $ commitList ++ [0]) $ \i ->
                        checkout i $ \_ ->
                            timed makeTime "make" j i $ cmd_ make ["-j" ++ show j] (EchoStdout False) stderr

                -- now with rattle
                when ("rattle" `elemOrNull` step) $ do
                    putStrLn "BUILDING WITH RATTLE"
                    clean
                    whenM (doesDirectoryExist ".rattle") $
                        removeDirectoryRecursive ".rattle"
                    forM_ (counted $ commitList ++ [0]) $ \i ->
                        checkout i $ \commit -> do
                            file <- generateName vs commit
                            cmds <- lines <$> readFile' file
                            let opts = rattleOptions{rattleProcesses=j, rattleUI=Just RattleQuiet, rattleNamedDirs=[], rattleShare=False}
                            timed rattleTime "rattle" j i $ rattleRun opts $ forM_ cmds $ cmd rattle stderr

                make <- readIORef makeTime
                rattle <- readIORef rattleTime
                putStrLn $ "TOTALS: make = " ++ showDuration make ++ ", rattle = " ++ showDuration rattle
    copyFile (dir </> "vsmake.log") "vsmake.log"
