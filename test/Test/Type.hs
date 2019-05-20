{-# LANGUAGE ScopedTypeVariables #-}

module Test.Type(
    withOutput,
    ignoreIO
    ) where

import Development.Rattle
import Control.Exception
import System.Directory


withOutput :: (FilePath -> IO a) -> IO a
withOutput act = do
    initDataDirectory
    ignoreIO $ removeDirectoryRecursive "output"
    createDirectoryIfMissing True "output"
    withCurrentDirectory "output" $ act ".."

ignoreIO :: IO () -> IO ()
ignoreIO act = catch act $ \(_ :: IOException) -> return ()
