
-- | The information from Paths_rattle cleaned up
module General.Paths(
    rattleVersionString,
    readDataFileHTML
    ) where

import Paths_rattle
import Control.Monad.Extra
import Data.Version
import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Environment
import General.Extra
import qualified Data.ByteString.Lazy as LBS


rattleVersionString :: String
rattleVersionString = showVersion version


-- We want getDataFileName to be relative to the current directory on program startup,
-- even if we issue a change directory command. Therefore, first call caches, future ones read.
{-# NOINLINE dataDirs #-}
dataDirs :: [String]
dataDirs = unsafePerformIO $ do
    datdir <- getDataDir
    exedir <- takeDirectory <$> getExecutablePath `catchIO` \_ -> return ""
    curdir <- getCurrentDirectory
    return $ [datdir] ++ [exedir | exedir /= ""] ++ [curdir]


getDataFile :: FilePath -> IO FilePath
getDataFile file = do
    let poss = map (</> file) dataDirs
    res <- filterM doesFileExist_ poss
    case res of
        [] -> fail $ unlines $ ("Could not find data file " ++ file ++ ", looked in:") : map ("  " ++) poss
        x:_ -> return x


readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = LBS.readFile =<< getDataFile ("html" </> file)
