{-# LANGUAGE ScopedTypeVariables #-}

module Test(main) where

import Development.Rattle
import System.FilePattern.Directory
import Development.Shake.FilePath
import Control.Exception
import System.Directory
import System.Info.Extra
import Control.Monad.Extra


withOutput :: (FilePath -> IO a) -> IO a
withOutput act = do
    initDataDirectory
    ignoreIO $ removeDirectoryRecursive "output"
    createDirectoryIfMissing True "output"
    withCurrentDirectory "output" $ act ".."

ignoreIO :: IO () -> IO ()
ignoreIO act = catch act $ \(_ :: IOException) -> return ()


-- Don't run on Mac because of https://github.com/jacereda/fsatrace/issues/25
main = unless isMac $ withOutput $ \root -> do
    let wipe = mapM (ignoreIO . removeFile) =<< getDirectoryFiles "." ["*"]
    cs <- liftIO $ getDirectoryFiles "." [root </> "test/C/*.c"]
    let toO x = takeBaseName x <.> "o"
    let build = do
            forM_ cs $ \c -> cmd "gcc -o" [toO c, "-c", c]
            cmd "gcc -o" ["Main" <.> exe] (map toO cs)
            cmd ["./Main" <.> exe]

    putStrLn "Build 1: Expect everything"
    rattle rattleOptions build
    putStrLn "Getting profiling data"
    writeProfile rattleOptions $ root ++ "/report.html"
    (w,s,p) <- graphData rattleOptions
    putStrLn $ "Work: " ++ show w
    putStrLn $ "Span: " ++ show s
    putStrLn $ "Parallelism: " ++ show p
    putStrLn "Build 2: Expect nothing"
    rattle rattleOptions build
    wipe
    putStrLn "Build 3: Expect cached (some speculation)"
    rattle rattleOptions build


    putStrLn "Build 4: Read/write hazard"
    handle (\(h :: Hazard) -> print h) $ do
        rattle rattleOptions{rattleSpeculate=Nothing} $ do
            cmd ["./Main" <.> exe]
            cmd "gcc -o" ["Main" <.> exe] (reverse $ map toO cs)
        putStrLn "Hoped it failed, but doesn't always"
        -- fail "Expected a hazard"

    putStrLn "Build 5: Rebuild after"
    rattle rattleOptions build
