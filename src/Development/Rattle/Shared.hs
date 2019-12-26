{-# LANGUAGE DeriveGeneric #-}

module Development.Rattle.Shared(
    Shared, withShared,
    getSpeculate, setSpeculate,
    getFile, setFile,
    getCmdTraces, addCmdTrace,
    nextRun, lastRun,
    dump
    ) where

import General.Extra
import Development.Rattle.Types
import Development.Rattle.Hash
import General.FileName
import System.Directory.Extra
import System.FilePath
import System.IO.Extra
import Data.Maybe
import Data.List
import Control.Monad.Extra
import Text.Read
import Control.Concurrent.Extra
import qualified Data.ByteString as BS
import Data.Serialize
import GHC.Generics
import Data.Either
---------------------------------------------------------------------
-- PRIMITIVES

data Shared = Shared Lock FilePath

withShared :: FilePath -> (Shared -> IO a) -> IO a
withShared dir act = do
    lock <- newLock
    createDirectoryRecursive dir
    act $ Shared lock dir

filename :: Hash -> String
filename (Hash str) = let (a:b:cs) = show str in
                        [a,b] </> cs

getList :: (Show a, Serialize b) => String -> Shared -> a -> IO [b]
getList typ (Shared lock dir) name = withLock lock $ do
    let file = dir </> typ </> filename (hashString $ show name)
    b <- doesFileExist file
    if not b then return [] else fromRight [] . decode <$> BS.readFile file

setList :: (Show a, Serialize b) => String -> IOMode -> Shared -> a -> [b] -> IO ()
setList typ mode (Shared lock dir) name vals = withLock lock $ do
    let file = dir </> typ </> filename (hashString $ show name)
    createDirectoryRecursive $ takeDirectory file
    unlessM (doesFileExist $ file <.> "txt") $
        writeFile (file <.> "txt") $ show name
    withFile file mode $ \h -> do
        hSetEncoding h utf8
        BS.hPutStr h $ encode vals

---------------------------------------------------------------------
-- SPECIAL SUPPORT FOR FILES

getFile :: Shared -> Hash -> IO (Maybe (FilePath -> IO ()))
getFile (Shared lock dir) hash = do
    let file = dir </> "files" </> filename hash
    b <- doesFileExist file
    return $ if not b then Nothing else Just $ \out -> do
        createDirectoryRecursive $ takeDirectory out
        copyFile file out

setFile :: Shared -> FilePath -> Hash -> IO Bool -> IO ()
setFile (Shared lock dir) source hash check = do
    let file = dir </> "files" </> filename hash
    b <- doesFileExist file
    unlessM (doesFileExist file) $ withLock lock $ do
        createDirectoryRecursive $ takeDirectory file
        copyFile source (file <.> "tmp")
        good <- check
        if not good then
            removeFile $ file <.> "tmp"
         else
            renameFile (file <.> "tmp") file


---------------------------------------------------------------------
-- TYPE SAFE WRAPPERS

nextRun :: Shared -> String -> IO RunIndex
nextRun share name = do
    t <- maybe runIndex0 nextRunIndex . listToMaybe <$> getList "run" share name
    setList "run" WriteMode share name [t]
    return t

lastRun :: Shared -> String -> IO (Maybe RunIndex)
lastRun share name = listToMaybe <$> getList "run" share name

getSpeculate :: Shared -> String -> IO [Cmd]
getSpeculate = getList "speculate"

setSpeculate :: Shared -> String -> [Cmd] -> IO ()
setSpeculate = setList "speculate" WriteMode

-- Intermediate data type which puts spaces in the right places to get better
-- word orientated diffs when looking at the output in a text editor
data File = File FileName BS.ByteString
    deriving (Show,Read,Generic)

instance Serialize File

getCmdTraces :: Shared -> Cmd -> IO [Trace (FileName, Hash)]
getCmdTraces shared cmd = map (fmap fromFile) <$> getList "command" shared cmd
    where fromFile (File path x) = (path, Hash x)

addCmdTrace :: Shared -> Cmd -> Trace (FileName, Hash) -> IO ()
addCmdTrace share cmd t = setList "command" AppendMode share cmd [fmap toFile t]
    where toFile (path, Hash x) = File path x


---------------------------------------------------------------------
-- DUMPING

dumpList :: (String -> IO ()) -> FilePath -> String -> IO ()
dumpList out dir name = do
    out ""
    out $ "## " ++ name
    dirs <- listDirectories $ dir </> name
    forM_ dirs $ \x -> do
        files <- filter (".txt" `isSuffixOf`) <$> listFiles x
        forM_ files $ \file -> do
            out ""
            name <- readFileUTF8' file
            out $ "### " ++ name
            out =<< readFileUTF8' (dropExtension file)


dump :: (String -> IO ()) -> FilePath -> IO ()
dump out dir = do
    out $ "# Rattle dump: " ++ dir
    mapM_ (dumpList out dir) ["run","speculate","command"]
