
module Test(main) where

import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.FilePath
import General.Paths
import Development.Rattle

import qualified Test.Example.FSATrace
import qualified Test.Example.Stack
import qualified Test.Limit
import qualified Test.Simple
import qualified Test.Trace

tests = let (*) = (,) in
    ["trace" * Test.Trace.main
    ,"limit" * Test.Limit.main
    ,"simple" * Test.Simple.main
    ,"fsatrace" * Test.Example.FSATrace.main
    ,"stack" * Test.Example.Stack.main
    ,"dump" * dump
    ]

main = do
    initDataDirectory
    args <- getArgs
    case args of
        [] ->
            forM_ tests $ \(name, act) -> do
                putStrLn $ "\n# Test " ++ name
                runAct name act
        name:args
            | Just act <- lookup (dropWhileEnd isDigit name) tests -> do
                putStrLn $ "\n# Test " ++ name
                withArgs args $ runAct name act
        _ -> do
            putStrLn $ "Unknown arguments, expected one of\n  " ++ unwords (map fst tests)
            exitFailure
    where
        runAct name act = do
            let dir = "output" </> name
            createDirectoryIfMissing True dir
            withCurrentDirectory dir act


dump :: IO ()
dump = do
    xs <- getArgs
    forM_ xs $ \x -> do
        withCurrentDirectory ".." $
            withFile (x </> "dump.rattle") WriteMode $ \h ->
                rattleDump (hPutStrLn h) $ x </> ".rattle"
        putStrLn $ "Dump written to " ++ x </> "dump.rattle"
