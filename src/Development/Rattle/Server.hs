{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, ViewPatterns, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module Development.Rattle.Server(
    Rattle, withRattle, Run(..),
    Hazard(..), Recoverable(..),
    addCmdOptions
    ) where

import Control.Monad.Extra
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import General.Pool
import Development.Rattle.Types
import Development.Rattle.UI
import Development.Rattle.Shared
import Development.Rattle.Options
import Development.Rattle.Hash
import Development.Rattle.Hazards
import Development.Rattle.CmdOption
import Control.Exception.Extra
import Control.Concurrent.Extra
import General.Extra
import Data.Either
import Data.Maybe
import System.Directory
import System.FilePath
import System.FilePattern
import System.IO.Extra
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Development.Shake.Command as C
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.IORef
import Data.List.Extra
import Data.Tuple.Extra
import System.Time.Extra


-- | Type of actions to run. Executed using 'rattle'.
newtype Run a = Run {fromRun :: ReaderT Rattle IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance a ~ () => C.CmdArguments (Run a) where
    cmdArguments (C.CmdArgument x) = do
        let (opts, args) = partitionEithers x
        r <- Run ask
        liftIO $ cmdRattle r opts args



data S = S
    {started :: Map.HashMap Cmd (NoShow (IO ()))
        -- ^ Things that have got to running - if you find a duplicate just run the IO
        --   to wait for it.
    ,running :: [(Seconds, Cmd, Touch FilePath)]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: HazardSet
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(Seconds, Cmd, Trace (FilePath, Hash))]
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

data Rattle = Rattle
    {options :: RattleOptions
    ,speculate :: [(Cmd, [Trace FilePath])] -- ^ Things that were used in the last speculation with this name
    ,runNum :: !RunIndex -- ^ Run# we are on
    ,state :: Var (Either Problem S)
    ,timer :: IO Seconds
    ,speculated :: IORef Bool
    ,pool :: Pool
    ,ui :: UI
    ,shared :: Shared
    }

addCmdOptions :: [C.CmdOption] -> Rattle -> Rattle
addCmdOptions new r@Rattle{options=o@RattleOptions{rattleCmdOptions=old}} =
    r{options = o{rattleCmdOptions = old ++ new}}


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withUI rattleFancyUI (return "Running") $ \ui -> withShared rattleFiles $ \shared -> do
    options@RattleOptions{..} <- rattleOptionsExplicit options

    speculate <- maybe (return []) (getSpeculate shared) rattleSpeculate
    speculate <- fmap (takeWhile (not . null . snd)) $ -- don't speculate on things we have no traces for
        forM speculate $ \x ->
            (x,) . map (fmap fst) <$> unsafeInterleaveIO (map (fmap $ first $ expand rattleNamedDirs) <$> getCmdTraces shared x)
    speculated <- newIORef False

    runNum <- nextRun shared rattleMachine
    timer <- offsetTime
    let s0 = Right $ S Map.empty [] emptyHazardSet [] []
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
        if not (recoverableHazard h || restartableHazard h) then throwIO h else do
            -- if we speculated, and we failed with a hazard, try again
            putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
            print h
            state <- newVar s0
            withPool rattleProcesses $ \pool -> do
                let r = Rattle{speculate=[], ..}
                (act r <* saveSpeculate state) `finally` writeVar state (Left Finished)

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
    | otherwise = step (addTrace (Set.empty, Set.empty) $ foldMap thd3 running) speculate
    where
        addTrace (r,w) Touch{..} = (f r tRead, f w tWrite)
            where f set xs = Set.union set $ Set.fromList xs

        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw@(r, w) ((x, foldMap tTouch -> t@Touch{..}):xs)
            | not $ any (\v -> v `Set.member` r || v `Set.member` w || seenHazardSet v hazard) tWrite
                -- if anyone I write has ever been read or written, or might be by an ongoing thing, that would be bad
            , not $ any (`Set.member` w) tRead
                -- if anyone I read might be being written right now, that would be bad
                = Just x
            | otherwise
                = step (addTrace rw t) xs


cmdRattle :: Rattle -> [C.CmdOption] -> [String] -> IO ()
cmdRattle rattle opts args = cmdRattleRequired rattle $ Cmd (rattleCmdOptions (options rattle) ++ opts) args

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
    start <- timer
    case Map.lookup cmd (started s) of
        Just (NoShow wait) -> return (Right s, wait)
        Nothing -> do
            hist <- unsafeInterleaveIO $ map (fmap $ first $ expand $ rattleNamedDirs options) <$> getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs
            s <- return s{running = (start, cmd, foldMap (fmap fst) $ map tTouch hist) : running s}
            s <- return s{started = Map.insert cmd (NoShow go) $ started s}
            return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)


-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> Seconds -> [Trace (FilePath, Hash)] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd opts args) startTimestamp hist msgs = do
    hasher <- memoIO hashFileForward
    let match (fp, h) = (== Just h) <$> hasher fp
    histRead <- filterM (allM match . tRead . tTouch) hist
    histBoth <- filterM (allM match . tWrite . tTouch) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle startTimestamp cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, h) = do v <- fetcher h; case v of Nothing -> return Nothing; Just op -> return $ Just $ op fp
            download <- if not (rattleShare options)
                then return Nothing
                else firstJustM (\t -> fmap (t,) <$> allMaybeM fetch (tWrite $ tTouch t)) histRead
            case download of
                Just (t, download) -> do
                    display ["copying"] $ sequence_ download
                    cmdRattleFinished rattle startTimestamp cmd t False
                Nothing -> do
                    start <- timer
                    (opts2, c) <- display [] $ cmdRattleRaw ui opts args
                    stop <- timer
                    t <- fsaTrace c
                    checkHashForwardConsistency t
                    let pats = matchMany [((), x) | Ignored xs <- opts2, x <- xs]
                    let skip x = "/dev/" `isPrefixOf` x || hasTrailingPathSeparator x || pats [((),x)] /= []
                    let f hasher xs = mapMaybeM (\x -> fmap (x,) <$> hasher x) $ filter (not . skip) xs
                    t <- Trace runNum start stop <$> (Touch <$> f hashFileForward (tRead t) <*> f hashFile (tWrite t))
                    x <- generateHashForwards cmd [x | HashNonDeterministic xs <- opts2, x <- xs] t
                    when (rattleShare options) $
                        forM_ (tWrite $ tTouch t) $ \(fp, h) ->
                            setFile shared fp h ((== Just h) <$> hashFile fp)
                    cmdRattleFinished rattle startTimestamp cmd t True
    where
        display :: [String] -> IO a -> IO a
        display msgs2 = addUI ui (head $ overrides ++ [cmdline]) (unwords $ msgs ++ msgs2)
        overrides = [x | C.Traced x <- opts] ++ [x | C.UserCommand x <- opts]
        cmdline = unwords $ ["cd " ++ x ++ " &&" | C.Cwd x <- opts] ++ args


cmdRattleRaw :: UI -> [C.CmdOption] -> [String] -> IO ([CmdOption2], [C.FSATrace])
cmdRattleRaw ui opts args = do
    (opts, opts2) <- return $ partitionEithers $ map fromCmdOption opts
    case [x | WriteFile x <- opts2] of
        [] -> do
            let optsUI = if isControlledUI ui then [C.EchoStdout False,C.EchoStderr False] else []
            res <- C.cmd (opts ++ optsUI) args
            return (opts2, res)
        files -> do
            forM_ files $ \file -> do
                createDirectoryIfMissing True $ takeDirectory file
                writeFileUTF8 file $ concat args
            return (opts2, map C.FSAWrite files)

checkHashForwardConsistency :: Touch FilePath -> IO ()
checkHashForwardConsistency Touch{..} = do
    -- check that anyone who is writing forwarding hashes is writing the actual file
    let sources = mapMaybe fromHashForward tWrite
    let bad = sources \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the forwarding file, but not the source: " ++ show bad

    -- and anyone writing to a file with a hash also updates it
    forwards <- filterM doesFileExist $ mapMaybe toHashForward tWrite
    let bad = forwards \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the source file which has a forwarding hash, but didn't touch the hash: " ++ show bad


-- | If you hae been asked to generate a forwarding hash for writes
generateHashForwards :: Cmd -> [FilePattern] -> Trace (FilePath, Hash) -> IO (Trace (FilePath, Hash))
generateHashForwards cmd ms t = do
    let match = matchMany $ map ((),) ms
    let (normal, forward) = partition (\(x, _) -> isJust (toHashForward x) && null (match [((), x)])) $ tWrite $ tTouch t
    let Hash hash = hashString $ show (cmd, tRead $ tTouch t, normal)
    let hhash = hashHash $ Hash hash
    forward <- forM forward $ \(x,_) -> do
        let Just x2 = toHashForward x -- checked this is OK earlier
        writeFile x2 hash
        return (x2, hhash)
    let addFwd t = t{tWrite = tWrite t ++ forward}
    return $ t{tTouch = addFwd $ tTouch t}

-- | I finished running a command
cmdRattleFinished :: Rattle -> Seconds -> Cmd -> Trace (FilePath, Hash) -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> do
        -- update all the invariants
        stop <- timer
        s <- return s{running = filter ((/= start) . fst3) $ running s}
        s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s}

        -- look for hazards
        -- push writes to the end, and reads to the start, because reads before writes is the problem
        let newHazards = newHazardSet start stop cmd $ fmap fst tTouch
        case mergeHazardSet (required s) (map fst speculate) (hazard s) newHazards of
            (ps@(p:_), _) -> return (Left $ Hazard p, print ps >> throwIO p)
            ([], hazard2) -> do
                s <- return s{hazard = hazard2}

                -- move people out of pending if they have survived long enough
                maxTimestamp <- timer
                let earliest = minimum $ maxTimestamp : map fst3 (running s)
                (safe, pending) <- return $ partition (\x -> fst3 x < earliest) $ pending s
                s <- return s{pending = pending}
                return (Right s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c $ fmap (first $ shorten (rattleNamedDirs options)) t)
