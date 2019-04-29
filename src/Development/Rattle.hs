{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

-- | General rules for writing consistent rattle build systems:
--
-- * Never write to the same file twice. Never read then write.
--
-- * Don't delete files that have been produced. Each command should make
--   new files, not delete old files.
module Development.Rattle(
    rattle, Run,
    Hazard,
    RattleOptions(..), rattleOptions,
    cmd,
    parallel, forP,
    liftIO, writeProfile, graphData,
    initDataDirectory
    ) where

import Control.Concurrent.Async
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Either.Extra
import General.Paths
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
forP :: (a -> Run b) -> [a] -> Run [b]
forP f xs = parallel $ map f xs


instance a ~ () => CmdArguments (Run a) where
    cmdArguments (CmdArgument x) = case partitionEithers x of
        (opts, x:xs) -> do
            r <- Run ask
            liftIO $ cmdRattle r opts x xs
        _ -> error "Error, no executable or arguments given to Development.Rattle.cmd"

-- | Given an Action to run, and a list of previous commands that got run, run it again
rattle :: RattleOptions -> Run a -> IO a
rattle opts (Run act) = withRattle opts $ \r ->
    runReaderT act r
