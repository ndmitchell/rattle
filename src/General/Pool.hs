{-# LANGUAGE TupleSections #-}

-- | Thread pool implementation. The three names correspond to the following
--   priority levels (highest to lowest):
--
-- * 'addPoolException' - things that probably result in a build error,
--   so kick them off quickly.
--
-- * 'addPoolResume' - things that started, blocked, and may have open
--   resources in their closure.
--
-- * 'addPoolStart' - rules that haven't yet started.
--
-- * 'addPoolBatch' - rules that might batch if other rules start first.
module General.Pool(
    Pool, runPool,
    addPool, addPoolWait, PoolPriority(..),
    ) where

import Control.Concurrent.Extra
import General.Thread
import System.Time.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import qualified Data.Heap as Heap
import qualified Data.HashSet as Set
import Data.IORef.Extra
import System.Random


---------------------------------------------------------------------
-- THREAD POOL

{-
Must keep a list of active threads, so can raise exceptions in a timely manner
If any worker throws an exception, must signal to all the other workers
-}

data S = S
    {alive :: !Bool -- True until there's an exception, after which don't spawn more tasks
    ,threads :: !(Set.HashSet Thread) -- IMPORTANT: Must be strict or we leak thread stacks
    ,threadsLimit :: {-# UNPACK #-} !Int -- user supplied thread limit, Set.size threads <= threadsLimit
    ,threadsCount :: {-# UNPACK #-} !Int -- Set.size threads, but in O(1)
    ,threadsMax :: {-# UNPACK #-} !Int -- high water mark of Set.size threads (accounting only)
    ,threadsSum :: {-# UNPACK #-} !Int -- number of threads we have been through (accounting only)
    ,rand :: IO Int -- operation to give us the next random Int
    ,todo :: !(Heap.Heap (Heap.Entry (PoolPriority, Int) (IO ()))) -- operations waiting a thread
    }


emptyS :: Int -> Bool -> IO S
emptyS n deterministic = do
    rand <- if not deterministic then return randomIO else do
        ref <- newIORef 0
        -- no need to be thread-safe - if two threads race they were basically the same time anyway
        return $ do i <- readIORef ref; writeIORef' ref (i+1); return i
    return $ S True Set.empty n 0 0 0 rand Heap.empty


data Pool = Pool
    !(Var S) -- Current state, 'alive' = False to say we are aborting
    !(Barrier (Either SomeException S)) -- Barrier to signal that we are finished


withPool :: Pool -> (S -> IO (S, IO ())) -> IO ()
withPool (Pool var _) f = join $ modifyVar var $ \s ->
    if alive s then f s else return (s, return ())

withPool_ :: Pool -> (S -> IO S) -> IO ()
withPool_ pool act = withPool pool $ fmap (, return()) . act


worker :: Pool -> IO ()
worker pool = withPool pool $ \s -> return $ case Heap.uncons $ todo s of
    Nothing -> (s, return ())
    Just (Heap.Entry _ now, todo2) -> (s{todo = todo2}, now >> worker pool)

-- | Given a pool, and a function that breaks the S invariants, restore them.
--   They are only allowed to touch threadsLimit or todo.
--   Assumes only requires spawning a most one job (e.g. can't increase the pool by more than one at a time)
step :: Pool -> (S -> IO S) -> IO ()
-- mask_ is so we don't spawn and not record it
step pool@(Pool _ done) op = uninterruptibleMask_ $ withPool_ pool $ \s -> do
    s <- op s
    -- evaluate s
    -- BS.putStrLn $ BS.pack $ show ("Pool of " , threadsLimit s, threadsCount s)
    case Heap.uncons $ todo s of
        Just (Heap.Entry _ now, todo2) | threadsCount s < threadsLimit s -> do
            -- spawn a new worker
            t <- newThreadFinally (now >> worker pool) $ \t res -> do
              case res of
                -- just cause someone gets an exception, doesn't mean we die now
                Left e | False -> withPool_ pool $ \s -> do
                    signalBarrier done $ Left e
                    return (remThread t s){alive = False}
                _ ->
                    step pool $ return . remThread t
            return (addThread t s){todo = todo2}
        -- rattle doesn't terminate when we run out of threads
        Nothing | False, threadsCount s == 0 -> do
            signalBarrier done $ Right s
            return s{alive = False}
        _ -> return s
    where
        addThread t s = s{threads = Set.insert t $ threads s, threadsCount = threadsCount s + 1
                         ,threadsSum = threadsSum s + 1, threadsMax = threadsMax s `max` (threadsCount s + 1)}
        remThread t s = s{threads = Set.delete t $ threads s, threadsCount = threadsCount s - 1}


-- | Add a new task to the pool. See the top of the module for the relative ordering
--   and semantics.
addPool :: PoolPriority -> Pool -> IO a -> IO ()
addPool priority pool act = step pool $ \s -> do
    i <- rand s
    return s{todo = Heap.insert (Heap.Entry (priority, i) $ void act) $ todo s}


-- | Somewhat dubious. Safe if the waiter gets killed if the pool gets torn down, which we assume happens.
addPoolWait :: PoolPriority -> Pool -> IO a -> IO a
addPoolWait priority pool act = do
    bar <- newBarrier
    addPool priority pool $ uninterruptibleMask $ \unmask -> do
        signalBarrier bar =<< try_ (unmask act)
    res <- waitBarrier bar
    either throwIO return res


data PoolPriority
    = PoolRequired
    | PoolSpeculate
      deriving (Eq,Ord)

-- | Run all the tasks in the pool on the given number of works.
--   If any thread throws an exception, the exception will be reraised.
runPool :: Bool -> Int -> (Pool -> IO a) -> IO a -- run all tasks in the pool
runPool deterministic n act = do
    s <- newVar =<< emptyS n deterministic
    done <- newBarrier
    let pool = Pool s done

    -- if someone kills our thread, make sure we kill our child threads
    let cleanup =
            join $ modifyVar s $ \s -> return (s{alive=False}, stopThreads $ Set.toList $ threads s)

    let ghc10793 = do
            -- if this thread dies because it is blocked on an MVar there's a chance we have
            -- a better error in the done barrier, and GHC raised the exception wrongly, see:
            -- https://ghc.haskell.org/trac/ghc/ticket/10793
            sleep 1 -- give it a little bit of time for the finally to run
                    -- no big deal, since the blocked indefinitely takes a while to fire anyway
            res <- waitBarrierMaybe done
            case res of
                Just (Left e) -> throwIO e
                _ -> throwIO BlockedIndefinitelyOnMVar
    flip finally cleanup $ handle (\BlockedIndefinitelyOnMVar -> ghc10793) $ do
        -- changes from Shake
        -- being in the pool doesn't consume a pool resource
        act pool
        -- we don't want for the pool to go quiet before continue, just as soon as the action dies
        -- so we remove the alive = False status
