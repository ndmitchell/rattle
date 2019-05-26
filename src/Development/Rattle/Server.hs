{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, ViewPatterns, LambdaCase #-}

module Development.Rattle.Server(
    RattleOptions(..), rattleOptions,
    Rattle, withRattle,
    Hazard(..), Recoverable(..),
    addCmdOptions, cmdRattle
    ) where

import Control.Monad.Extra
import General.Pool
import Development.Rattle.Types
import Development.Rattle.UI
import Development.Rattle.Shared
import Development.Rattle.Hash
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import System.FilePath
import System.FilePattern
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.Hashable
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra
import Control.Monad.IO.Class


-- | Basic options for configuring rattle.
data RattleOptions = RattleOptions
    {rattleFiles :: FilePath -- ^ Where all my shared files go
    ,rattleSpeculate :: Maybe String -- ^ Should I speculate? Under which key?
    ,rattleMachine :: String -- ^ Key to store run#
    ,rattleShare :: Bool -- ^ Should I share files from the cache
    ,rattleProcesses :: Int -- ^ Number of simulateous processes
    ,rattleCmdOptions :: [C.CmdOption] -- ^ Extra options added to every command line
    ,rattleIgnore :: [FilePattern] -- ^ Rattle files to ignore
    } deriving Show

-- | Default 'RattleOptions' value.
rattleOptions :: RattleOptions
rattleOptions = RattleOptions ".rattle" (Just "") "m1" True 0 [] []


rattleOptionsExplicit :: RattleOptions -> IO RattleOptions
rattleOptionsExplicit o = do
    o <- if rattleProcesses o /= 0 then return o else do p <- getProcessorCount; return o{rattleProcesses=p}
    return o


data ReadOrWrite = Read | Write deriving (Show,Eq)

data S = S
    {timestamp :: !T
        -- ^ The current timestamp we are on
    ,started :: Map.HashMap Cmd (NoShow (IO ()))
        -- ^ Things that have got to running - if you find a duplicate just run the IO
        --   to wait for it.
    ,running :: [(T, Cmd, [Trace FilePath])]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: Map.HashMap FilePath (ReadOrWrite, T, Cmd)
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(T, Cmd, Trace (FilePath, Hash))]
        -- ^ Things that have completed, and would like to get recorded, but have to wait
        --   to confirm they didn't cause hazards
    ,required :: [Cmd]
        -- ^ Things what were required by the user calling cmdRattle, not added due to speculation.
        --   Will be the 'speculate' list next time around.
    } deriving Show


data Problem
    = Finished
    | Hazard Hazard

throwProblem :: Problem -> IO a
throwProblem Finished = fail "Finished, but still trying to do stuff"
throwProblem (Hazard h) = throwIO h

-- | Type of exception thrown if there is a hazard when running the build system.
data Hazard
    = ReadWriteHazard FilePath Cmd Cmd Recoverable
    | WriteWriteHazard FilePath Cmd Cmd
      deriving Show
instance Exception Hazard

data Recoverable = Recoverable | NonRecoverable deriving (Show,Eq)

data Rattle = Rattle
    {options :: RattleOptions
    ,speculate :: [(Cmd, [Trace FilePath])] -- ^ Things that were used in the last speculation with this name
    ,runNum :: !T -- ^ Run# we are on
    ,state :: Var (Either Problem S)
    ,speculated :: IORef Bool
    ,pool :: Pool
    ,ui :: UI
    ,shared :: Shared
    }

addCmdOptions :: [C.CmdOption] -> Rattle -> Rattle
addCmdOptions new r@Rattle{options=o@RattleOptions{rattleCmdOptions=old}} =
    r{options = o{rattleCmdOptions = old ++ new}}


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withUI (return "Running") $ \ui -> withShared rattleFiles $ \shared -> do
    options@RattleOptions{..} <- rattleOptionsExplicit options

    speculate <- maybe (return []) (getSpeculate shared) rattleSpeculate
    speculate <- fmap (takeWhile (not . null . snd)) $ -- don't speculate on things we have no traces for
        forM speculate $ \x ->
            (x,) . map (fmap fst) <$> unsafeInterleaveIO (getCmdTraces shared x)
    speculated <- newIORef False

    runNum <- nextRun shared rattleMachine
    let s0 = Right $ S t0 Map.empty [] Map.empty [] []
    state <- newVar s0

    let saveSpeculate state =
            whenJust rattleSpeculate $ \name ->
                whenRightM (readVar state) $ \v ->
                    setSpeculate shared name $ reverse $ required v

    -- first try and run it
    let attempt1 = withPool rattleProcesses $ \pool -> do
            let r = Rattle{..}
            runSpeculate r
            (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)
    attempt1 `catch` \(h :: Hazard) -> do
        b <- readIORef speculated
        if not $ recoverableHazard h then throwIO h else do
            -- if we speculated, and we failed with a hazard, try again
            putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
            print h
            state <- newVar s0
            withPool rattleProcesses $ \pool -> do
                let r = Rattle{speculate=[], ..}
                (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)


recoverableHazard :: Hazard -> Bool
recoverableHazard WriteWriteHazard{} = False
recoverableHazard (ReadWriteHazard _ _ _ r) = r == Recoverable

runSpeculate :: Rattle -> IO ()
runSpeculate rattle@Rattle{..} = void $ forkIO $ void $ runPoolMaybe pool $
    -- speculate on a process iff it is the first process in speculate that:
    -- 1) we have some parallelism free
    -- 2) it is the first eligible in the list
    -- 3) not already been started
    -- 4) no read/write conflicts with anything completed
    -- 5) no read conflicts with anything running or any earlier speculation
    join $ modifyVar state $ \s -> case s of
        Right s | Just cmd <- nextSpeculate rattle s -> do
            writeIORef speculated True
            cmdRattleStarted rattle cmd s ["speculative"]
        _ -> return (s,  return ())


nextSpeculate :: Rattle -> S -> Maybe Cmd
nextSpeculate Rattle{..} S{..}
    | any (null . thd3) running = Nothing
    | otherwise = step (addTrace (Set.empty, Set.empty) $ mconcat $ concatMap thd3 running) speculate
    where
        addTrace (r,w) Trace{..} = (f r tRead, f w tWrite)
            where f set xs = Set.union set $ Set.fromList xs

        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw@(r, w) ((x, mconcat -> t@Trace{..}):xs)
            | not $ any (\v -> v `Set.member` r || v `Set.member` w || v `Map.member` hazard) tWrite
                -- if anyone I write has ever been read or written, or might be by an ongoing thing, that would be bad
            , not $ any (`Set.member` w) tRead
                -- if anyone I read might be being written right now, that would be bad
                = Just x
            | otherwise
                = step (addTrace rw t) xs


cmdRattle :: Rattle -> [C.CmdOption] -> String -> [String] -> IO ()
cmdRattle rattle opts exe args = cmdRattleRequired rattle $ Cmd (rattleCmdOptions (options rattle) ++ opts) exe args

cmdRattleRequired :: Rattle -> Cmd -> IO ()
cmdRattleRequired rattle@Rattle{..} cmd = runPool pool $ do
    modifyVar_ state $ return . fmap (\s -> s{required = cmd : required s})
    cmdRattleStart rattle cmd

cmdRattleStart :: Rattle -> Cmd -> IO ()
cmdRattleStart rattle@Rattle{..} cmd = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> cmdRattleStarted rattle cmd s []

cmdRattleStarted :: Rattle -> Cmd -> S -> [String] -> IO (Either Problem S, IO ())
cmdRattleStarted rattle@Rattle{..} cmd s msgs = do
    let start = timestamp s
    s <- return s{timestamp = succ $ timestamp s}
    case Map.lookup cmd (started s) of
        Just (NoShow wait) -> return (Right s, wait)
        Nothing -> do
            hist <- unsafeInterleaveIO $ getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs
            s <- return s{running = (start, cmd, map (fmap fst) hist) : running s}
            s <- return s{started = Map.insert cmd (NoShow go) $ started s}
            return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)


-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> T -> [Trace (FilePath, Hash)] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd opts exe args) start hist msgs = do
    hasher <- memoIO hashFile
    let match (fp, h) = (== Just h) <$> hasher fp
    histRead <- filterM (allM match . tRead) hist
    histBoth <- filterM (allM match . tWrite) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle start cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, h) = do v <- fetcher h; case v of Nothing -> return Nothing; Just op -> return $ Just $ op fp
            download <- if not (rattleShare options)
                then return Nothing
                else firstJustM (\t -> fmap (t,) <$> allMaybeM fetch (tWrite t)) histRead
            case download of
                Just (t, download) -> do
                    display ["copying"] $ sequence_ download
                    cmdRattleFinished rattle start cmd t False
                Nothing -> do
                    timer <- liftIO offsetTime
                    let opts2 = if isControlledUI ui then [C.EchoStdout False,C.EchoStderr False] else []
                    c <- display [] $ C.cmd (opts ++ opts2) exe args
                    end <- timer
                    t <- fsaTrace end runNum c
                    let pats = matchMany (map ((),) $ rattleIgnore options)
                    let skip x = "/dev/" `isPrefixOf` x || hasTrailingPathSeparator x || pats [((),x)] /= []
                    let f xs = mapMaybeM (\x -> fmap (x,) <$> hashFile x) $ filter (not . skip) xs
                    t <- Trace (tTime t) (tRun t) <$> f (tRead t) <*> f (tWrite t)
                    when (rattleShare options) $
                        forM_ (tWrite t) $ \(fp, h) ->
                            setFile shared fp h ((== Just h) <$> hashFile fp)
                    cmdRattleFinished rattle start cmd t True
    where
        display msgs2 = addUI ui (unwords $ cwd ++ exe : args) (unwords $ msgs ++ msgs2)
        cwd = ["cd " ++ x ++ " &&" | C.Cwd x <- opts]

-- | I finished running a command
cmdRattleFinished :: Rattle -> T -> Cmd -> Trace (FilePath, Hash) -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> do
        -- update all the invariants
        let stop = timestamp s
        s <- return s{timestamp = succ $ timestamp s}
        s <- return s{running = filter ((/= start) . fst3) $ running s}
        s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s}

        -- look for hazards
        -- push writes to the end, and reads to the start, because reads before writes is the problem
        let newHazards = Map.fromList $ map ((,(Write,stop ,cmd)) . fst) tWrite ++
                                        map ((,(Read ,start,cmd)) . fst) tRead
        case unionWithKeyEithers (mergeFileOps (required s) (map fst speculate)) (hazard s) newHazards of
            (ps@(p:_), _) -> return (Left $ Hazard p, print ps >> throwIO p)
            ([], hazard2) -> do
                s <- return s{hazard = hazard2}

                -- move people out of pending if they have survived long enough
                let earliest = minimum $ succ stop : map fst3 (running s)
                (safe, pending) <- return $ partition (\x -> fst3 x < earliest) $ pending s
                s <- return s{pending = pending}
                return (Right s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c t)

-- r is required list; s is speculate list
mergeFileOps :: [Cmd] -> [Cmd] -> FilePath -> (ReadOrWrite, T, Cmd) -> (ReadOrWrite, T, Cmd) -> Either Hazard (ReadOrWrite, T, Cmd)
mergeFileOps r s x (Read, t1, cmd1) (Read, t2, cmd2) = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps r s x (Write, t1, cmd1) (Write, t2, cmd2) = Left $ WriteWriteHazard x cmd1 cmd2
mergeFileOps r s x (Read, t1, cmd1) (Write, t2, cmd2)
    | listedBefore cmd1 cmd2 = Left $ ReadWriteHazard x cmd2 cmd1 NonRecoverable
    | t1 <= t2 = Left $ ReadWriteHazard x cmd2 cmd1 Recoverable
    | otherwise = Right (Write, t2, cmd2)
  where -- FIXME: listedBefore is O(n) so want to make that partly cached
        listedBefore c1 c2 = let i1 = elemIndex c1 r
                                 i2 = elemIndex c2 r in
                               f i1 i2 c1 c2
        f Nothing Nothing c1 c2 = let Just i1 = elemIndex c1 s -- both should be in speculate list
                                      Just i2 = elemIndex c2 s
                                  in i1 < i2 -- speculate list is reverse of required
        f (Just i1) (Just i2) _ _ = i1 > i2
        f (Just i1) Nothing _ _ = True -- 2nd one isn't in required list so it must be listed after i1
        f Nothing (Just i2) _ _ = False -- first one isn't in required list so it must be listed after i2
mergeFileOps r s x v1 v2 = mergeFileOps r s x v2 v1 -- must be Write/Read, so match the other way around


allMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allMaybeM f [] = return $ Just []
allMaybeM f (x:xs) = do
    y <- f x
    case y of
        Nothing -> return Nothing
        Just y -> fmap (y:) <$> allMaybeM f xs


unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e v) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp)
            Just v1 -> case op k v1 v2 of
                Left e -> (e:es, mp)
                Right v -> (es, Map.insert k v mp)
