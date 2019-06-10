{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

-- | General rules for writing consistent rattle build systems:
--
-- * Never write to the same file twice. Never read then write.
--
-- * Don't delete files that have been produced. Each command should make
--   new files, not delete old files.
module Development.Rattle(
    rattleRun, Run,
    rattleDump,
    Hazard,
    RattleOptions(..), rattleOptions,
    cmd, CmdOption(..), CmdOption2(..), toCmdOption, withCmdOptions,
    parallel, forP, forP_,
    memo, memoRec,
    cmdWriteFile,
    liftIO, writeProfile, graphData
    ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent.Extra
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Development.Shake.Command
import Development.Rattle.Server
import Development.Rattle.Options
import Development.Rattle.CmdOption
import Development.Rattle.Shared
import Development.Rattle.Profile


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


cmdWriteFile :: FilePath -> String -> Run ()
cmdWriteFile file str = cmd (WriteFile file) [str]


-- | Apply specific options ot all nested Run values.
withCmdOptions :: [CmdOption] -> Run a -> Run a
withCmdOptions xs (Run act) = Run $ withReaderT (addCmdOptions xs) act

-- | Given an Action to run, and a list of previous commands that got run, run it again
rattleRun :: RattleOptions -> Run a -> IO a
rattleRun opts (Run act) = withRattle opts $ \r ->
    runReaderT act r

-- | Dunmp the contents of a shared cache
rattleDump :: (String -> IO ()) -> FilePath -> IO ()
rattleDump = dump

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
