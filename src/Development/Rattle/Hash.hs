{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Development.Rattle.Hash(
    Hash(..),
    hashFile, hashString, hashHash, hashHex,
    hashFileForward, toHashForward, fromHashForward,
    hashFileForwardIfStale, hashFileIfStale
    ) where

import System.IO
import Data.Hashable
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import System.IO.Unsafe
import Data.Char
import Numeric
import Control.Monad.Extra
import Data.IORef
import Control.Exception.Extra
import Control.DeepSeq
import GHC.Generics
import Data.Serialize
import General.FileName
import General.FileInfo

-- | A hash, encoded 32 bytes, may contain NUL or other funny characters
newtype Hash = Hash BS.ByteString
    deriving (NFData, Eq, Hashable, Generic)

instance Show Hash where
    show = hashHex

instance Serialize Hash

mkHash :: BS.ByteString -> Hash
mkHash = Hash

-- | Show a hash as hex characters
hashHex :: Hash -> String
hashHex (Hash x) = concatMap (f . ord) $ BS.unpack x
    where f i = ['0' | i < 16] ++ showHex i ""

-- Hashing lots of files is expensive, so we keep a cache
{-# NOINLINE hashCache #-}
hashCache :: IORef (Map.HashMap FileName (ModTime, Hash))
hashCache = unsafePerformIO $ newIORef Map.empty

toHashForward :: FileName -> Maybe FileName
toHashForward x = let b = fileNameToByteString x
                      s = BS.pack ".rattle.hash" in
                    if BS.isSuffixOf s b then Nothing
                    else Just $ byteStringToFileName $ BS.append b s

fromHashForward :: FileName -> Maybe FileName
fromHashForward x = let b = fileNameToByteString x
                        s = BS.pack ".rattle.hash" in
                      byteStringToFileName <$> BS.stripSuffix s b

hashFileForwardIfStale :: FileName -> ModTime -> Hash -> IO (Maybe Hash)
hashFileForwardIfStale file mt h =
  case toHashForward file of
    Nothing -> hashFileIfStale file mt h
    Just file2 -> do
      b2 <- doesFileNameExist file2
      if not b2 then hashFileIfStale file mt h else do
        b <- doesFileNameExist file
        if not b then return Nothing else hashFileIfStale file2 mt h

hashFileIfStale :: FileName -> ModTime -> Hash -> IO (Maybe Hash)
hashFileIfStale file mt h = do
  start <- getModTime file
  case start of
    Nothing -> return Nothing
    Just start -> do
      mp <- readIORef hashCache
      case Map.lookup file mp of
        Just (time,hash) | time == start -> return $ Just hash
        _ | start == mt -> do f start h; return $ Just h
        _ -> do
          b <- doesFileNameExist file
          if not b then return Nothing else do
            res <- withFile (fileNameToString file) ReadMode $ \h -> do
              chunks <- LBS.hGetContents h
              evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
            end <- getModTime file
            when (Just start == end) $
              f start res
            return $ Just res
    where f start res = atomicModifyIORef' hashCache $ \mp -> (Map.insert file (start, res) mp, ())

-- | If there is a forwarding hash, and this file exists, use the forwarding hash instead
hashFileForward :: FileName -> IO (Maybe (ModTime, Hash))
hashFileForward file =
    case toHashForward file of
        Nothing -> hashFile file
        Just file2 -> do
            b2 <- doesFileNameExist file2
            if not b2 then hashFile file else do
                b <- doesFileNameExist file
                if not b then return Nothing else hashFile file2

hashFile :: FileName -> IO (Maybe (ModTime, Hash))
hashFile file = do
    start <- getModTime file
    case start of
        Nothing -> return Nothing
        Just start -> do
            mp <- readIORef hashCache
            case Map.lookup file mp of
                Just (time, hash) | time == start -> return $ Just (time, hash)
                _ -> do
                    -- we can get a ModTime on a directory, but can't withFile it
                    b <- doesFileNameExist file
                    if not b then return Nothing else do
                        res <- withFile (fileNameToString file) ReadMode $ \h -> do
                            chunks <- LBS.hGetContents h
                            evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
                        end <- getModTime file
                        when (Just start == end) $
                            atomicModifyIORef' hashCache $ \mp -> (Map.insert file (start, res) mp, ())
                        return $ Just (start, res)


hashString :: String -> Hash
-- we first 'show' the String to avoid having > 256 characters in it
hashString = mkHash . SHA.hash . BS.pack . show


hashHash :: Hash -> Hash
hashHash (Hash x) = mkHash $ SHA.hash x
