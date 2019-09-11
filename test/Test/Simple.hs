{-# LANGUAGE ScopedTypeVariables #-}

module Test.Simple(main) where

import Test.Type
import Development.Rattle
import System.FilePattern.Directory
import Development.Shake.FilePath
import Control.Exception
import System.Directory
import System.Info.Extra
import Control.Monad.Extra


-- Don't run on Mac because of https://github.com/jacereda/fsatrace/issues/25
main = unless isMac $ do
    let wipe = mapM (ignoreIO . removeFile) =<< getDirectoryFiles "." ["*"]
    cs <- liftIO $ getDirectoryFiles "." [root </> "test/C/*.c"]
    let toO x = takeBaseName x <.> "o"
    let build = do
            forM_ cs $ \c -> cmd "gcc -o" [toO c, "-c", c]
            cmd "gcc -o" ["Main" <.> exe] (map toO cs)
            cmd ["./Main" <.> exe]

    putStrLn "Build 1: Expect everything"
    rattleRun rattleOptions build
    putStrLn "Getting profiling data"
    writeProfile rattleOptions $ root ++ "/report.html"
    (w,s,p) <- graphData rattleOptions
    putStrLn $ "Work: " ++ show w
    putStrLn $ "Span: " ++ show s
    putStrLn $ "Parallelism: " ++ show p
    putStrLn "Build 2: Expect nothing"
    rattleRun rattleOptions build
    wipe
    putStrLn "Build 3: Expect cached (some speculation)"
    rattleRun rattleOptions build


    putStrLn "Build 4: Read/write hazard"
    handle (\(h :: Hazard) -> print h) $ do
        rattleRun rattleOptions{rattleSpeculate=Nothing} $ do
            cmd ["./Main" <.> exe]
            cmd "gcc -o" ["Main" <.> exe] (reverse $ map toO cs)
        putStrLn "Hoped it failed, but doesn't always"
        -- fail "Expected a hazard"

    putStrLn "Build 5: Rebuild after"
    rattleRun rattleOptions build

    putStrLn "Build 6: Rebuild from a different directory"
    createDirectoryIfMissing True "inner"
    withCurrentDirectory "inner" $
        rattleRun rattleOptions $ withCmdOptions [Cwd ".."]  build
    putStrLn "Build 7: Cause Restartable hazard"
    rattleRun rattleOptions $
      forM_ cs $ \c -> cmd "touch" c -- count as a write
    putStrLn "Should cause restartable hazard"
    rattleRun rattleOptions build
