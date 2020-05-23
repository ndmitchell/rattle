
-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Thread(
    Thread, newThreadFinally, stopThreads
    ) where

import Data.Hashable
import Control.Concurrent.Extra
import Control.Exception

data Thread = Thread ThreadId (Barrier ())

instance Eq Thread where
    Thread a _ == Thread b _ = a == b

instance Hashable Thread where
    hashWithSalt salt (Thread a _) = hashWithSalt salt a


-- | The inner thread is unmasked even if you started masked.
newThreadFinally :: IO a -> (Thread -> Either SomeException a -> IO ()) -> IO Thread
newThreadFinally act cleanup = do
    bar <- newBarrier
    t <- mask_ $ forkIOWithUnmask $ \unmask -> flip finally (signalBarrier bar ()) $ do
        res <- try $ unmask act
        me <- myThreadId
        cleanup (Thread me bar) res
    pure $ Thread t bar

stopThreads :: [Thread] -> IO ()
stopThreads threads = do
    -- if a thread is in a masked action, killing it may take some time, so kill them in parallel
    bars <- sequence [do forkIO $ killThread t; pure bar | Thread t bar <- threads]
    mapM_ waitBarrier bars
