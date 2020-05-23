
-- | Helpers built on top of Rattle
module Development.Rattle.Derived(
    parallel, forP, forP_,
    withCmdOptions,
    memo, memoRec,
    cmdWriteFile,
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
import Development.Rattle.CmdOption


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
cmdWriteFile file str = cmd (Traced $ "Writing file " ++ file) (WriteFile file) [str]


-- | Apply specific options ot all nested Run values.
withCmdOptions :: [CmdOption] -> Run a -> Run a
withCmdOptions xs (Run act) = Run $ withReaderT (addCmdOptions xs) act

-- | Memoize an IO action
memo :: (Eq a, Hashable a, MonadIO m) => (a -> m b) -> m (a -> m b)
memo f = memoRec $ const f

-- | Memoize an IO action which is recursive
memoRec :: (Eq a, Hashable a, MonadIO m) => ((a -> m b) -> a -> m b) -> m (a -> m b)
memoRec f = do
    var <- liftIO $ newVar Map.empty
    let go x =
            join $ liftIO $ modifyVar var $ \mp -> case Map.lookup x mp of
                Just bar -> pure (mp, liftIO $ waitBarrier bar)
                Nothing -> do
                    bar <- newBarrier
                    pure (Map.insert x bar mp, do v <- f go x; liftIO $ signalBarrier bar v; pure v)
    pure go
