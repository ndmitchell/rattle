{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

-- | General rules for writing consistent rattle build systems:
--
-- * Never write to the same file twice. Never read then write.
--
-- * Don't delete files that have been produced. Each command should make
--   new files, not delete old files.
module Development.Rattle(
    rattleRun, Run,
    Hazard,
    RattleOptions(..), rattleOptions,
    cmd, CmdOption(..), withCmdOptions,
    parallel, forP, forP_,
    memo, memoRec,
    liftIO, writeProfile, graphData
    ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Data.Either.Extra
import Control.Concurrent.Extra
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Development.Shake.Command
import Development.Rattle.Server
import Development.Rattle.Profile


-- | Type of actions to run. Executed using 'rattle'.
newtype Run a = Run {fromRun :: ReaderT Rattle IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

-- | Run a sequence of 'Run' actions in parallel. They will be run in parallel with no limit
--   on simultaneous executions.
parallel :: [Run a] -> Run [a]
parallel xs = do
    r <- Run ask
    liftIO $ mapConcurrently (flip runReaderT r . fromRun) xs

-- | Parallel version of 'forM'.
forP :: [a] -> (a -> Run b) -> Run [b]
forP xs f = parallel $ map f xs

-- | Parallel version of 'forM'.
forP_ :: [a] -> (a -> Run b) -> Run ()
forP_ xs f = void $ forP xs f


-- | Apply specific options ot all nested Run values.
withCmdOptions :: [CmdOption] -> Run a -> Run a
withCmdOptions xs (Run act) = Run $ withReaderT (addCmdOptions xs) act

instance a ~ () => CmdArguments (Run a) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> do
            r <- Run ask
            liftIO $ cmdRattle r opts x xs
        _ -> error "Error, no executable or arguments given to Development.Rattle.cmd"

-- | Given an Action to run, and a list of previous commands that got run, run it again
rattleRun :: RattleOptions -> Run a -> IO a
rattleRun opts (Run act) = withRattle opts $ \r ->
    runReaderT act r

-- | Memoize an IO action
memo :: (Eq a, Hashable a, MonadIO m) => (a -> m b) -> m (a -> m b)
memo f = memoRec $ const f

-- | Memoize an IO action which is recursive
memoRec :: (Eq a, Hashable a, MonadIO m) => ((a -> m b) -> a -> m b) -> m (a -> m b)
memoRec f = do
    var <- liftIO $ newVar Map.empty
    let go x =
            join $ liftIO $ modifyVar var $ \mp -> case Map.lookup x mp of
                Just bar -> return (mp, liftIO $ waitBarrier bar)
                Nothing -> do
                    bar <- newBarrier
                    return (Map.insert x bar mp, do v <- f go x; liftIO $ signalBarrier bar v; return v)
    return go
