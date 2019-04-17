
module General.Extra(
    whenLeft,
    whenRightM,
    tryIO,
    createDirectoryRecursive,
    NoShow(..),
    memoIO
    ) where

import Control.Exception.Extra
import System.Directory
import Data.IORef
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Control.Monad


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

tryIO :: IO a -> IO (Either IOException a)
tryIO = try


---------------------------------------------------------------------
-- System.Directory

-- | Like @createDirectoryIfMissing True@ but faster, as it avoids
--   any work in the common case the directory already exists.
createDirectoryRecursive :: FilePath -> IO ()
createDirectoryRecursive dir = do
    x <- tryIO $ doesDirectoryExist dir
    when (x /= Right True) $ createDirectoryIfMissing True dir


---------------------------------------------------------------------
-- Data.Either

whenLeft :: Applicative m => Either a b -> (a -> m ()) -> m ()
whenLeft x f = either f (const $ pure ()) x


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
