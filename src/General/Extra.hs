
module General.Extra(
    whenRightM,
    createDirectoryRecursive, doesFileExist_,
    NoShow(..),
    memoIO, catchIO,
    getProcessorCount
    ) where

import Control.Exception.Extra
import System.Directory
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Control.Monad
import System.IO.Unsafe
import Control.Concurrent
import System.Environment
import Data.List
import System.IO.Extra
import GHC.Conc(getNumProcessors)



---------------------------------------------------------------------
-- Prelude

newtype NoShow a = NoShow a
instance Show (NoShow a) where show _ = "NoShow"


---------------------------------------------------------------------
-- Control.Monad

whenRightM :: Monad m => m (Either l r) -> (r -> m ()) -> m ()
whenRightM x act =  either (const $ return ()) act =<< x


---------------------------------------------------------------------
-- Control.Exception

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


---------------------------------------------------------------------
-- System.Directory

doesFileExist_ :: FilePath -> IO Bool
doesFileExist_ x = doesFileExist x `catchIO` \_ -> return False

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
    x <- tryIO $ doesDirectoryExist dir
    when (x /= Right True) $ createDirectoryIfMissing True dir


---------------------------------------------------------------------
-- Data.Memo

memoIO :: (Hashable k, Eq k) => (k -> IO v) -> IO (k -> IO v)
memoIO f = do
    ref <- newIORef Map.empty
    return $ \k -> do
        mp <- readIORef ref
        case Map.lookup k mp of
            Just v -> return v
            Nothing -> do
                v <- f k
                atomicModifyIORef ref $ \mp -> (Map.insert k v mp, v)


---------------------------------------------------------------------
-- System.Info

-- Copied from Shake
{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in return res
    where
        act =
            if rtsSupportsBoundThreads then
                fromIntegral <$> getNumProcessors
            else do
                env <- lookupEnv "NUMBER_OF_PROCESSORS"
                case env of
                    Just s | [(i,"")] <- reads s -> return i
                    _ -> do
                        src <- readFile' "/proc/cpuinfo" `catchIO` \_ -> return ""
                        return $! max 1 $ length [() | x <- lines src, "processor" `isPrefixOf` x]
