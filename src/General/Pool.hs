{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

-- | Pools of workers, guarantees cleanup.
module General.Pool(
    Pool,
    withPool,
    runPool,
    runPoolMaybe
    ) where

import Control.Exception
import General.Limit
import Control.Monad.Extra
import Control.Concurrent.Extra
import Control.Concurrent.Async
import qualified Data.HashSet as Set


data Pool = Pool
    Limit -- For limiting how many people are running
    (Var (Maybe (Set.HashSet (Async ())))) -- things that are running, or Nothing for cleaning up


withPool :: Int -> (Pool -> IO a) -> IO a
withPool n act = do
    limit <- newLimit n
    threads <- newVar $ Just Set.empty
    act (Pool limit threads) `finally2` do
        threads <- modifyVar threads $ \(Just v) -> return (Nothing, v)
        mapConcurrently cancel $ Set.toList threads


a `finally2` sequel =
  uninterruptibleMask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r


spawn :: Var (Maybe (Set.HashSet (Async ()))) -> IO a -> IO a
spawn var act = mask $ \restore1 -> uninterruptibleMask $ \restore2 -> do
    -- Important we do uninterruptibleMask since spawning async is interruptible
    -- and after we have spawn'd async its important it ends up in the mp so it can be cancelled
    -- However, relax that during cleanup, since if someone wants to abort cleanup its their choice
    (thing, after) <- modifyVar var $ \case
        Nothing -> return (Nothing, (fail "runPool after pool has terminated", return ()))
        Just mp -> do
            a <- async $ restore1 act
            let undo = modifyVar_ var $ \case
                    Nothing -> return Nothing
                    Just mp -> do
                        mp <- evaluate $ Set.delete (void a) mp
                        return $ Just mp
            mp <- evaluate $ Set.insert (void a) mp
            return (Just mp, (wait a, cancel a >> undo))
    r <- restore1 thing `onException` restore2 after
    restore2 after
    return r


runPool :: Pool -> IO a -> IO a
runPool (Pool limit threads) = withLimit limit . spawn threads

runPoolMaybe :: Pool -> IO a -> IO (Maybe a)
runPoolMaybe (Pool limit threads) = withLimitMaybe limit . spawn threads
