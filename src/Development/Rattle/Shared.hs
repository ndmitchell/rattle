
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
import Control.Concurrent.Extra
import qualified Data.ByteString as BS
import General.FileInfo
import General.Binary
import Data.Monoid
import Prelude

---------------------------------------------------------------------
-- PRIMITIVES

data Shared = Shared Lock FilePath Bool

withShared :: FilePath -> Bool -> (Shared -> IO a) -> IO a
withShared dir multiple act = do
    lock <- newLock
    createDirectoryRecursive dir
    act $ Shared lock dir multiple

filenameHash :: Hash -> String
filenameHash str = let (a:b:cs) = hashHex str in [a,b] </> cs

filenameValue :: BinaryEx a => a -> String
filenameValue = filenameHash . hashByteString . runBuilder . putEx

getList :: (BinaryEx a, BinaryEx b) => String -> Shared -> a -> IO [b]
getList typ (Shared lock dir _) name = withLock lock $ do
    let file = dir </> typ </> filenameValue name
    b <- doesFileExist file
    if not b then pure [] else map getEx . getExList <$> BS.readFile file

setList :: (Show a, BinaryEx a, BinaryEx b) => String -> IOMode -> Shared -> a -> [b] -> IO ()
setList typ mode (Shared lock dir multiple) name vals = withLock lock $ do
    let mode2 = if multiple then mode else WriteMode
    let file = dir </> typ </> filenameValue name
    createDirectoryRecursive $ takeDirectory file
    unlessM (doesFileExist $ file <.> "txt") $
        writeFile (file <.> "txt") $ show name
    withFile file mode2 $ \h -> do
        hSetEncoding h utf8
        BS.hPutStr h $ runBuilder $ putExList $ map putEx vals

---------------------------------------------------------------------
-- SPECIAL SUPPORT FOR FILES

getFile :: Shared -> Hash -> IO (Maybe (FileName -> IO ()))
getFile (Shared lock dir _) hash = do
    let file = dir </> "files" </> filenameHash hash
    b <- doesFileExist file
    pure $ if not b then Nothing else Just $ \out -> do
      let x = fileNameToString out
      createDirectoryRecursive $ takeDirectory x
      copyFile file x

setFile :: Shared -> FileName -> Hash -> IO Bool -> IO ()
setFile (Shared lock dir _) source hash check = do
    let file = dir </> "files" </> filenameHash hash
    unlessM (doesFileExist file) $ withLock lock $ do
        createDirectoryRecursive $ takeDirectory file
        copyFile (fileNameToString source) (file <.> "tmp")
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
    pure t

lastRun :: Shared -> String -> IO (Maybe RunIndex)
lastRun share name = listToMaybe <$> getList "run" share name

getSpeculate :: Shared -> String -> IO [Cmd]
getSpeculate = getList "speculate"

setSpeculate :: Shared -> String -> [Cmd] -> IO ()
setSpeculate = setList "speculate" WriteMode

-- Intermediate data type which puts spaces in the right places to get better
-- word orientated diffs when looking at the output in a text editor
data File = File FileName ModTime Hash
    deriving (Show)

instance BinaryEx File where
    getEx x = File (byteStringToFileName a) b (getEx c)
        where (b,ca) = binarySplit x
              (c,a) = BS.splitAt hashLength ca
    putEx (File a b c) = putExStorable b <> putEx c <> putEx (fileNameToByteString a)


-- First trace in list is earliest one; last is latest one.
getCmdTraces :: Shared -> Cmd -> IO [Trace (FileName, ModTime, Hash)]
getCmdTraces shared cmd = map (fmap fromFile) <$> getList "command" shared cmd
    where fromFile (File path mt x) = (path, mt, x)

addCmdTrace :: Shared -> Cmd -> Trace (FileName, ModTime, Hash) -> IO ()
addCmdTrace share cmd t = setList "command" AppendMode share cmd [fmap toFile t]
    where toFile (path, mt, x) = File path mt x


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
