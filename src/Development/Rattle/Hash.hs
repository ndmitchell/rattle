{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Rattle.Hash(
    Hash(..),
    hashFile, hashString,
    hashFileForward, toHashForward, fromHashForward
    ) where

import System.IO
import Data.Hashable
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import Data.Time
import System.Directory
import Data.Char
import System.IO.Unsafe
import System.IO.Error
import Control.Monad.Extra
import Data.IORef
import Data.List.Extra
import System.FilePath
import Numeric
import Control.Exception.Extra
import Control.DeepSeq


newtype Hash = Hash String
    deriving (NFData, Show, Read, Eq, Hashable)


mkHash :: BS.ByteString -> Hash
mkHash = Hash . concatMap (f . ord) . BS.unpack
    where f i = ['0' | i < 16] ++ showHex i ""


-- Hashing lots of files is expensive, so we keep a cache
{-# NOINLINE hashCache #-}
hashCache :: IORef (Map.HashMap FilePath (UTCTime, Hash))
hashCache = unsafePerformIO $ newIORef Map.empty


getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime x = handleBool isDoesNotExistError (const $ return Nothing) (Just <$> getModificationTime x)

toHashForward :: FilePath -> Maybe FilePath
toHashForward x | ".rattle.hash" `isSuffixOf` x = Nothing
                | otherwise = Just $ x <.> "rattle.hash"

fromHashForward :: FilePath -> Maybe FilePath
fromHashForward x | Just x <- stripSuffix ".rattle.hash" x = Just x
                  | otherwise = Nothing

-- | If there is a forwarding hash, and this file exists, use the forwarding hash instead
hashFileForward :: FilePath -> IO (Maybe Hash)
hashFileForward file =
    case toHashForward file of
        Nothing -> hashFile file
        Just file2 -> do
            b2 <- doesFileExist file2
            if not b2 then hashFile file else do
                b <- doesFileExist file
                if not b then return Nothing else hashFile file2

hashFile :: FilePath -> IO (Maybe Hash)
hashFile file = do
    start <- getModTime file
    case start of
        Nothing -> return Nothing
        Just start -> do
            mp <- readIORef hashCache
            case Map.lookup file mp of
                Just (time, hash) | time == start -> return $ Just hash
                _ -> do
                    -- we can get a ModTime on a directory, but can't withFile it
                    b <- doesFileExist file
                    if not b then return Nothing else do
                        res <- withFile file ReadMode $ \h -> do
                            chunks <- LBS.hGetContents h
                            evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
                        end <- getModTime file
                        when (Just start == end) $
                            atomicModifyIORef' hashCache $ \mp -> (Map.insert file (start, res) mp, ())
                        return $ Just res


hashString :: String -> Hash
-- we first 'show' the String to avoid having > 256 characters in it
hashString = mkHash . SHA.hash . BS.pack . show
