
module General.Extra(
    whenRightM, allMaybeM,
    createDirectoryRecursive, doesFileExist_,
    NoShow(..),
    memoIO, catchIO,
    getProcessorCount,
    unionWithKeyEithers, insertWithKeyEithers
    ) where

import Control.Exception.Extra
import System.Directory
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Control.Monad.Extra
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
whenRightM x act = eitherM (const $ pure ()) act x

allMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allMaybeM f [] = pure $ Just []
allMaybeM f (x:xs) = do
    y <- f x
    case y of
        Nothing -> pure Nothing
        Just y -> fmap (y:) <$> allMaybeM f xs


---------------------------------------------------------------------
-- Control.Exception

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


---------------------------------------------------------------------
-- System.Directory

doesFileExist_ :: FilePath -> IO Bool
doesFileExist_ x = doesFileExist x `catchIO` \_ -> pure False

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
    pure $ \k -> do
        mp <- readIORef ref
        case Map.lookup k mp of
            Just v -> pure v
            Nothing -> do
                v <- f k
                atomicModifyIORef ref $ \mp -> (Map.insert k v mp, v)


---------------------------------------------------------------------
-- System.Info

-- Copied from Shake
{-# NOINLINE getProcessorCount #-}
getProcessorCount :: IO Int
-- unsafePefromIO so we cache the result and only compute it once
getProcessorCount = let res = unsafePerformIO act in pure res
    where
        act =
            if rtsSupportsBoundThreads then
                fromIntegral <$> getNumProcessors
            else do
                env <- lookupEnv "NUMBER_OF_PROCESSORS"
                case env of
                    Just s | [(i,"")] <- reads s -> pure i
                    _ -> do
                        src <- readFile' "/proc/cpuinfo" `catchIO` \_ -> pure ""
                        pure $! max 1 $ length [() | x <- lines src, "processor" `isPrefixOf` x]


---------------------------------------------------------------------
-- Data.HashMap

unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e (Maybe v)) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs = insertWithKeyEithers op lhs . Map.toList

insertWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e (Maybe v)) -> Map.HashMap k v -> [(k,v)] -> ([e], Map.HashMap k v)
insertWithKeyEithers op lhs = foldl' f ([], lhs)
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp)
            Just v1 -> case op k v1 v2 of
                Left e -> (e:es, mp)
                Right Nothing -> (es, mp)
                Right (Just v) -> (es, Map.insert k v mp)
