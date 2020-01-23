{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections, LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, ViewPatterns #-}

module Development.Rattle.Server(
    Rattle, withRattle, Run(..),
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
import General.FileName
import General.FileInfo
import qualified Data.ByteString.Char8 as BS
import Data.Hashable

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
    ,running :: [(Seconds, Cmd, Touch FileName)]
        -- ^ Things currently running, with the time they started,
        --    and an amalgamation of their previous Trace (if we have any)
    ,hazard :: HazardSet
        -- ^ Things that have been read or written, at what time, and by which command
        --   Used to detect hazards.
        --   Read is recorded as soon as it can, Write as late as it can, as that increases hazards.
    ,pending :: [(Seconds, Cmd, Trace (FileName, ModTime, Hash))]
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
    ,speculate :: [(Cmd, Touch FileName)] -- ^ Things that were used in the last speculation with this name
    ,runIndex :: !RunIndex -- ^ Run# we are on
    ,state :: Var (Either Problem S)
    ,timer :: IO Seconds
    ,speculated :: IORef Bool
    ,pool :: Pool
    ,ui :: UI
    ,shared :: Shared
    ,debugFile :: Var (Maybe Handle)
    }

addCmdOptions :: [C.CmdOption] -> Rattle -> Rattle
addCmdOptions new r@Rattle{options=o@RattleOptions{rattleCmdOptions=old}} =
    r{options = o{rattleCmdOptions = old ++ new}}


withRattle :: RattleOptions -> (Rattle -> IO a) -> IO a
withRattle options@RattleOptions{..} act = withUI rattleFancyUI (return "Running") $ \ui -> withShared rattleFiles $ \shared -> do
    options@RattleOptions{..} <- rattleOptionsExplicit options

    speculate <- maybe (return []) (getSpeculate shared) rattleSpeculate
    speculate <- fmap (takeWhile (not . null . snd)) $ -- don't speculate on things we have no traces for
        forM speculate $ \x -> do
            traces <- unsafeInterleaveIO (getCmdTraces shared x)
            return (x, normalizeTouch $ foldMap (fmap (expand rattleNamedDirs . fst3) . tTouch) traces)
    speculated <- newIORef False

    runIndex <- nextRun shared rattleMachine
    timer <- offsetTime
    let s0 = Right $ S Map.empty [] emptyHazardSet [] []
    state <- newVar s0
    debugFile <- newVar =<< case rattleDebug of
                              Nothing -> return Nothing
                              Just f -> Just <$> openFile f WriteMode

    let saveSpeculate state =
            whenJust rattleSpeculate $ \name ->
                whenRightM (readVar state) $ \v ->
                    setSpeculate shared name $ reverse $ required v

    -- first try and run it
    let attempt1 = withPool rattleProcesses $ \pool -> do
            let r = Rattle{..}
            runSpeculate r
            (act r <* saveSpeculate state) `finally` (writeVar state (Left Finished) >> withVar debugFile (\case
                                                                                                              Nothing -> return ()
                                                                                                              Just h -> hClose h))
    attempt1 `catch` \(h :: Hazard) -> do
        b <- readIORef speculated
        if not (recoverableHazard h || restartableHazard h) then throwIO h else do
            -- if we speculated, and we failed with a hazard, try again
            putStrLn "Warning: Speculation lead to a hazard, retrying without speculation"
            print h
            state <- newVar s0
            withPool rattleProcesses $ \pool -> do
                let r = Rattle{speculate=[], ..}
                (act r <* saveSpeculate state) `finally` (writeVar state (Left Finished) >> withVar debugFile (\case
                                                                                                                  Nothing -> return ()
                                                                                                                  Just h -> hClose h))

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

        step :: (Set.HashSet FileName, Set.HashSet FileName) -> [(Cmd, Touch FileName)] -> Maybe Cmd
        step _ [] = Nothing
        step rw ((x,_):xs)
            | x `Map.member` started = step rw xs -- do not update the rw, since its already covered
        step rw@(r, w) ((x, t@Touch{..}):xs)
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
            hist <- unsafeInterleaveIO $ map (fmap (\(f,mt,h) -> (expand (rattleNamedDirs options) f, mt, h))) <$> getCmdTraces shared cmd
            go <- once $ cmdRattleRun rattle cmd start hist msgs
            s <- return s{running = (start, cmd, foldMap (fmap fst3 . tTouch) hist) : running s}
            s <- return s{started = Map.insert cmd (NoShow go) $ started s}
            return (Right s, runSpeculate rattle >> go >> runSpeculate rattle)


-- first is true; 2nd is false
helper :: Monad m => (a -> m (Maybe b)) -> [a] -> m ([a], [b])
helper _ [] = return ([], [])
helper f (x:xs) = do
  res <- f x
  (as,bs) <- helper f xs
  case res of
    Nothing -> return (x:as, bs)
    Just y -> return (as, y:bs)

listToMaybeSet :: (Eq a, Hashable a) => [a] -> Maybe (Set.HashSet a)
listToMaybeSet [] = Nothing
listToMaybeSet ls = Just $ Set.fromList ls

-- either fetch it from the cache or run it)
cmdRattleRun :: Rattle -> Cmd -> Seconds -> [Trace (FileName, ModTime, Hash)] -> [String] -> IO ()
cmdRattleRun rattle@Rattle{..} cmd@(Cmd opts args) startTimestamp hist msgs = do
    let match (fp, mt, h) = (== Just h) <$> hashFileForwardIfStale fp mt h
    (histRead, changedR) <- helper (fmap listToMaybeSet . filterM match . tWrite . tTouch) hist
    (histBoth, changedW) <- helper (fmap listToMaybeSet . filterM match . tWrite . tTouch) histRead
    case histBoth of
        t:_ ->
            -- we have something consistent at this point, no work to do
            -- technically we aren't writing to the tWrite part of the trace, but if we don't include that
            -- skipping can turn write/write hazards into read/write hazards
            cmdRattleFinished rattle startTimestamp cmd t False
        [] -> do
            -- lets see if any histRead's are also available in the cache
            fetcher <- memoIO $ getFile shared
            let fetch (fp, mt, h) = do v <- fetcher h; case v of Nothing -> return Nothing; Just op -> return $ Just $ op fp
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
                    touch <- fsaTrace c
                    checkHashForwardConsistency touch
                    let pats = matchMany [((), x) | Ignored xs <- opts2, x <- xs]
                    --let hasTrailingPathSeparator x = if BS.null x then False else isPathSeparator $ BS.last x
                    let skip x = let y = fileNameToString x in
                                   isPrefixOf "/dev/" y || hasTrailingPathSeparator y || pats [((),y)] /= []
                    let f hasher xs = mapMaybeM (\x -> fmap (\(mt,h) -> (x,mt,h)) <$> hasher x) $ filter (not . skip) xs
                    touch <- Touch <$> f hashFileForward (tRead touch) <*> f hashFile (tWrite touch)
                    touch <- generateHashForwards cmd [x | HashNonDeterministic xs <- opts2, x <- xs] touch
                    --whenJust (rattleDebug options) $ \_ ->
                      --printCmdInfo changedR changedW touch

                    when (rattleShare options) $
                        forM_ (tWrite touch) $ \(fp, mt, h) ->
                            setFile shared fp h ((== Just h) <$> hashFileIfStale fp mt h)
                    cmdRattleFinished rattle startTimestamp cmd (Trace runIndex start stop touch) True
    where
        display :: [String] -> IO a -> IO a
        display msgs2 = addUI ui (head $ overrides ++ [cmdline]) (unwords $ msgs ++ msgs2)
        overrides = [x | C.Traced x <- opts] ++ [x | C.UserCommand x <- opts]
        cmdline = unwords $ ["cd " ++ x ++ " &&" | C.Cwd x <- opts] ++ args
        printCmdInfo :: [Set.HashSet (FileName, ModTime, Hash)] -> [Set.HashSet (FileName, ModTime, Hash)] -> Touch (FileName, ModTime, Hash) -> IO ()
        printCmdInfo changedR changedW touch =
          let ht = case hist of
                     [] -> Map.empty
                     (tWrite . tTouch . head -> x) -> Map.fromList [(fp,h) | (fp,_,h) <- x] in
            unless (null hist) $ do
            -- construct the string we want to write
            let cstr = "Cmd: " ++ show cmd ++ "\n"
                rstr = if null changedR
                       then if null changedW
                            then "Nothing changed causing cmd to run.\n"
                            else "Changed WRITE files caused cmd to run: " ++ show (map fst3 $ Set.toList $ head changedW) ++ " \n"
                       else "Changed READ files caused cmd to run: " ++ show (map fst3 $ Set.toList $ head changedR) ++ " \n"

                (sw,cw) = partition (\(fp,_,h) -> case Map.lookup fp ht of
                                                    (Just h2) -> h == h2
                                                    Nothing -> False) (tWrite touch)
                wstr1 = "Written files UNCHANGED after run: " ++ show (map fst3 sw) ++ " \n"
                wstr2 = "Written files CHANGED after run: " ++ show (map fst3 cw) ++ " "
                str = cstr ++ rstr ++ wstr1 ++ wstr2
            withVar debugFile ((`hPutStrLn` str) . fromJust)

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

checkHashForwardConsistency :: Touch FileName -> IO ()
checkHashForwardConsistency Touch{..} = do
    -- check that anyone who is writing forwarding hashes is writing the actual file
    let sources = mapMaybe fromHashForward tWrite
    let bad = sources \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the forwarding file, but not the source: " ++ show bad

    -- and anyone writing to a file with a hash also updates it
    forwards <- filterM doesFileNameExist $ mapMaybe toHashForward tWrite
    let bad = forwards \\ tWrite
    when (bad /= []) $
        fail $ "Wrote to the source file which has a forwarding hash, but didn't touch the hash: " ++ show bad


-- | If you have been asked to generate a forwarding hash for writes
generateHashForwards :: Cmd -> [FilePattern] -> Touch (FileName, ModTime, Hash) -> IO (Touch (FileName, ModTime, Hash))
generateHashForwards cmd ms t = do
    let match = matchMany $ map ((),) ms
    let (normal, forward) = partition (\(x, _, _) -> isJust (toHashForward x) && null (match [((), fileNameToString x)])) $ tWrite t
    let Hash hash = hashString $ show (cmd, tRead t, normal)
    let hhash = hashHash $ Hash hash
    forward <- forM forward $ \(x,mt,_) -> do
        let Just x2 = toHashForward x -- checked this is OK earlier
        BS.writeFile (fileNameToString x2) hash
        return (x2, mt,hhash)
    return t{tWrite = tWrite t ++ forward}

-- | I finished running a command
cmdRattleFinished :: Rattle -> Seconds -> Cmd -> Trace (FileName, ModTime, Hash) -> Bool -> IO ()
cmdRattleFinished rattle@Rattle{..} start cmd trace@Trace{..} save = join $ modifyVar state $ \case
    Left e -> throwProblem e
    Right s -> do
        -- update all the invariants
        stop <- timer
        s <- return s{running = filter ((/= start) . fst3) $ running s}
        s <- return s{pending = [(stop, cmd, trace) | save] ++ pending s}

        -- look for hazards
        -- push writes to the end, and reads to the start, because reads before writes is the problem
        let newHazards = newHazardSet start stop cmd $ fmap fst3 tTouch
        case mergeHazardSet (required s) (hazard s) newHazards of
            (ps@(p:_), _) -> return (Left $ Hazard p, print ps >> throwIO p)
            ([], hazard2) -> do
                s <- return s{hazard = hazard2}

                -- move people out of pending if they have survived long enough
                maxTimestamp <- timer
                let earliest = minimum $ maxTimestamp : map fst3 (running s)
                (safe, pending) <- return $ partition (\x -> fst3 x < earliest) $ pending s
                s <- return s{pending = pending}
                return (Right s, forM_ safe $ \(_,c,t) -> addCmdTrace shared c $ fmap (\(f,mt,h) -> (shorten (rattleNamedDirs options) f, mt,h)) t)
