{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Rattle.Hash(
    Hash(..), hashLength,
    hashFile, hashString, hashByteString, hashHash, hashHex,
    hashFileForward, toHashForward, fromHashForward,
    hashFileForwardIfStale, hashFileIfStale
    ) where

import System.IO
import Data.Hashable
import qualified Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString as BS8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as Map
import System.IO.Unsafe
import Data.Bits
import Control.Monad.Extra
import Data.IORef.Extra
import General.Binary
import Control.Exception.Extra
import Control.DeepSeq
import General.FileName
import General.FileInfo

-- | A hash, exactly 32 bytes, may contain NUL or other funny characters
newtype Hash = Hash BS.ByteString
    deriving (NFData, Eq, Hashable)

hashLength :: Int
hashLength = 32

instance BinaryEx Hash where
    getEx = Hash
    putEx (Hash x) = putEx x


instance Show Hash where
    show = hashHex

mkHash :: BS.ByteString -> Hash
mkHash = Hash

-- | Show a hash as hex characters
hashHex :: Hash -> String
hashHex (Hash x) = f $ BS8.unpack x
    where
        f (x:xs) = g (x `shiftR` 4) : g (x .&. 0xf) : f xs
        f [] = []
        g x = case x of
            0  -> '0'
            1  -> '1'
            2  -> '2'
            3  -> '3'
            4  -> '4'
            5  -> '5'
            6  -> '6'
            7  -> '7'
            8  -> '8'
            9  -> '9'
            10 -> 'a'
            11 -> 'b'
            12 -> 'c'
            13 -> 'd'
            14 -> 'e'
            15 -> 'f'

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
        if not b then pure Nothing else hashFileIfStale file2 mt h

hashFileIfStale :: FileName -> ModTime -> Hash -> IO (Maybe Hash)
hashFileIfStale file mt h = do
  start <- getModTime file
  case start of
    Nothing -> pure Nothing
    Just start -> do
      mp <- readIORef hashCache
      case Map.lookup file mp of
        Just (time,hash) | time == start -> pure $ Just hash
        _ | start == mt -> do f start h; pure $ Just h
        _ -> do
          b <- doesFileNameExist file
          if not b then pure Nothing else do
            res <- withFile (fileNameToString file) ReadMode $ \h -> do
              chunks <- LBS.hGetContents h
              evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
            end <- getModTime file
            when (Just start == end) $
              f start res
            pure $ Just res
    where f start res = atomicModifyIORef'_ hashCache $ Map.insert file (start, res)

-- | If there is a forwarding hash, and this file exists, use the forwarding hash instead
hashFileForward :: FileName -> IO (Maybe (ModTime, Hash))
hashFileForward file =
    case toHashForward file of
        Nothing -> hashFile file
        Just file2 -> do
            b2 <- doesFileNameExist file2
            if not b2 then hashFile file else do
                b <- doesFileNameExist file
                if not b then pure Nothing else hashFile file2

hashFile :: FileName -> IO (Maybe (ModTime, Hash))
hashFile file = do
    start <- getModTime file
    case start of
        Nothing -> pure Nothing
        Just start -> do
            mp <- readIORef hashCache
            case Map.lookup file mp of
                Just (time, hash) | time == start -> pure $ Just (time, hash)
                _ -> do
                    -- we can get a ModTime on a directory, but can't withFile it
                    b <- doesFileNameExist file
                    if not b then pure Nothing else do
                        res <- withFile (fileNameToString file) ReadMode $ \h -> do
                            chunks <- LBS.hGetContents h
                            evaluate $ force $ mkHash $ SHA.finalize $ SHA.updates SHA.init $ LBS.toChunks chunks
                        end <- getModTime file
                        when (Just start == end) $
                            atomicModifyIORef'_ hashCache $ Map.insert file (start, res)
                        pure $ Just (start, res)


hashString :: String -> Hash
hashString = hashByteString . BS.pack

hashByteString :: BS.ByteString -> Hash
hashByteString = mkHash . SHA.hash

hashHash :: Hash -> Hash
hashHash (Hash x) = mkHash $ SHA.hash x
