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
    act (Pool limit threads) `finally` do
        threads <- modifyVar threads $ \(Just v) -> return (Nothing, v)
        mapConcurrently cancel $ Set.toList threads


spawn :: Var (Maybe (Set.HashSet (Async ()))) -> IO a -> IO a
spawn var act = uninterruptibleMask $ \restore -> do
    -- Important we do uninterruptibleMask since spawning async is interruptible
    -- and after we have spawn'd async its important it ends up in the mp so it can be cancelled
    (thing, after) <- modifyVar var $ \case
        Nothing -> return (Nothing, (fail "runPool after pool has terminated", return ()))
        Just mp -> do
            a <- async $ restore act
            let undo = modifyVar_ var $ \case
                    Nothing -> return Nothing
                    Just mp -> do
                        mp <- evaluate $ Set.delete (void a) mp
                        return $ Just mp
            mp <- evaluate $ Set.insert (void a) mp
            return (Just mp, (wait a, cancel a >> undo))
    r <- restore thing `onException` after
    after
    return r


runPool :: Pool -> IO a -> IO a
runPool (Pool limit threads) = withLimit limit . spawn threads

runPoolMaybe :: Pool -> IO a -> IO (Maybe a)
runPoolMaybe (Pool limit threads) = withLimitMaybe limit . spawn threads
