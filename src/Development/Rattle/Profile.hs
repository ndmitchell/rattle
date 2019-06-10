{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -w #-}

module Development.Rattle.Profile(
  constructGraph, Graph(..), dotStringOfGraph,
  graphData, writeProfile
  ) where

import Development.Rattle.Server
import Development.Rattle.Options
import Development.Rattle.Types
import Development.Rattle.Hash
import Development.Rattle.Shared
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.ByteString.Lazy.Char8 as LBS
import General.Template
import General.Paths
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import System.Time.Extra
import Numeric.Extra

-- edge is directed based on order cmd were listed in script
-- end1 was listed before end2. helps determine read/write hazards
data Edge = Edge {end1 :: (Cmd, [Trace (FilePath, Hash)])
                 ,end2 :: (Cmd, [Trace (FilePath, Hash)])
                 ,hazard :: Maybe Hazard
                 }

data Graph = Graph {nodes :: [(Cmd, [Trace (FilePath, Hash)])]
                   ,edges :: [Edge]
                   }

instance Show Edge where
  show (Edge e1 e2 Nothing) = showCmd (fst e1) ++ " -> " ++ showCmd (fst e2)
  show (Edge e1 e2 (Just h)) = showCmd (fst e1) ++ " -> " ++ showCmd (fst e2) ++ " [ label=\"" ++ show h ++ "\" ];"

getCmdsTraces :: RattleOptions -> IO [(Cmd,[Trace (FilePath, Hash)])]
getCmdsTraces options@RattleOptions{..} = withShared rattleFiles$ \shared -> do
  cmds <- maybe (return []) (getSpeculate shared) rattleSpeculate
  fmap (takeWhile (not . null . snd)) $ forM cmds $ \x -> (x,) <$> getCmdTraces shared x

getLastRun :: RattleOptions -> IO (Maybe T)
getLastRun options@RattleOptions{..} = withShared rattleFiles $ \shared ->
  lastRun shared rattleMachine

constructGraph :: RattleOptions -> IO Graph
constructGraph options@RattleOptions{..} = do
  cmdsWTraces <- getCmdsTraces options
  return $ createGraph cmdsWTraces

-- | Given some options, produce various statistics.
graphData :: RattleOptions -> IO (Seconds,Seconds,Seconds)
graphData options = do
  cmdsWTraces <- getCmdsTraces options
  let graph = createGraph cmdsWTraces
      w = work graph
      s = spanGraph graph in
    return (w,s,w / s)

-- | Generate a profile report given a file.
writeProfile :: RattleOptions -> FilePath -> IO ()
writeProfile options out = do
  graph <- constructGraph options
  runNum <- getLastRun options
  writeProfileInternal out graph runNum

writeProfileInternal :: FilePath -> Graph -> Maybe T -> IO ()
writeProfileInternal out g t = LBS.writeFile out =<< generateHTML g t

{- build graph using trace info
   Add an edge between 2 nodes if they both write the same file
   Add an edge between 2 nodes if one reads a file and another writes it -}
createGraph :: [(Cmd,[Trace (FilePath, Hash)])] -> Graph
createGraph xs = Graph xs $ g xs
  where g [] = []
        g (x:xs) = let edges = mapMaybe (createEdge x) xs in
          edges ++ g xs

-- assume p1 occurred before p2.
-- find the worst type of hazard if there is an edge
createEdge :: (Cmd,[Trace (FilePath, Hash)]) -> (Cmd,[Trace (FilePath, Hash)]) -> Maybe Edge
createEdge p1@(cmd1,ts) p2@(cmd2,ls) = -- first look for write write hazard then look for both read/write and write/read hazards
  case writeWriteHazard ts ls of
    Just fp -> Just $ Edge p1 p2 $ Just $ WriteWriteHazard fp cmd1 cmd2
    Nothing -> case readWriteHazard ts ls of
                 Just fp -> Just $ Edge p1 p2 $ Just $ ReadWriteHazard fp cmd1 cmd2 NonRecoverable
                 Nothing -> -- check for a non hazard edge
                   case readWriteHazard ls ts of
                     Just fp -> Just $ Edge p1 p2 Nothing -- regular edge
                     Nothing -> Nothing -- no edge

-- Is there a writewrite edge?
writeWriteHazard :: [Trace (FilePath, Hash)] -> [Trace (FilePath, Hash)] -> Maybe FilePath
writeWriteHazard = maybeHazard tWrite

-- Is there a readwrite edge?
readWriteHazard :: [Trace (FilePath, Hash)] -> [Trace (FilePath, Hash)] -> Maybe FilePath
readWriteHazard = maybeHazard tRead

maybeHazard :: (Trace (FilePath, Hash) -> [(FilePath, Hash)]) -> [Trace (FilePath, Hash)] -> [Trace (FilePath, Hash)] -> Maybe FilePath
maybeHazard _ [] ls = Nothing
maybeHazard _ ls [] = Nothing
maybeHazard f (t:ts) ls =
  case find (\y -> isJust $ memberWrites y ls) $ f t of
    Just (fp,_) -> Just fp
    Nothing -> maybeHazard f ts ls

memberWrites :: (FilePath, Hash) -> [Trace (FilePath, Hash)] -> Maybe FilePath
memberWrites x [] = Nothing
memberWrites x@(fp,_) (y:ys) =
  case fmap (fp,) $ lookup fp $ tWrite y of
    Just (fp,_) -> Just fp
    Nothing -> memberWrites x ys

-- todo fix
generateDotString :: Graph -> IO String
generateDotString (Graph ns xs) = return $ "digraph " ++ "{\n" ++
                                  showEdges xs ++
                                  "\n}"

showEdges :: [Edge] -> String
showEdges [] = ""
showEdges (x:xs) = show x ++ "\n" ++ showEdges xs

showCmd :: Cmd -> String
showCmd (Cmd _ args) = show $ showCmdHelper args

showCmdHelper :: [String] -> String
showCmdHelper [] = ""
showCmdHelper (x:xs) = x ++ " " ++ showCmdHelper xs

dotStringOfGraph :: RattleOptions -> IO String
dotStringOfGraph options = do
  edges <- constructGraph options
  generateDotString edges

graphRoots :: [(Cmd,[Trace (FilePath, Hash)])] -> [Edge] -> [(Cmd,[Trace (FilePath, Hash)])]
graphRoots = foldr (delete . end2)

graphLeaves :: [(Cmd,[Trace (FilePath, Hash)])] -> [Edge] -> [(Cmd,[Trace (FilePath, Hash)])]
graphLeaves = foldr (delete . end1)

firstTTime :: [Trace (FilePath, Hash)] -> Seconds
firstTTime [] = 0
firstTTime xs = tTime $ head xs

work :: Graph -> Seconds
work (Graph ns es) = sum $ map (firstTTime . snd) ns

spanGraph :: Graph -> Seconds
spanGraph (Graph ns es) =
  -- get roots and calculate span for each root; take max
  -- roots are the cmds that are only end1;
  -- probably want a hashset from cmd to edges
  let cmds = foldl' (\m (Edge e1 e2 h) -> Map.insertWith (++) e1 [e2] m) Map.empty es
      roots = graphRoots ns es in
    foldl' (\m c -> max m $ spanCmd c cmds) 0.0 roots

spanCmd :: (Cmd, [Trace (FilePath, Hash)]) -> Map.HashMap (Cmd, [Trace (FilePath, Hash)]) [(Cmd,[Trace (FilePath, Hash)])] -> Seconds
spanCmd cmd@(c,ts) cmds =
  case Map.lookup cmd cmds of
    Nothing -> firstTTime ts
    Just ls -> firstTTime ts + foldl (\m c -> max m $ spanCmd c cmds) 0.0 ls

parallelism :: Graph -> Seconds
parallelism g = work g / spanGraph g

generateHTML :: Graph -> Maybe T -> IO LBS.ByteString
generateHTML xs t = do
  report <- readDataFileHTML "profile.html"
  let f "data/profile-data.js" = return $ LBS.pack $ "var profile =\n" ++ generateJSON xs t
  runTemplate f report

allWrites :: [Trace (FilePath, Hash)] -> [FilePath]
allWrites [] = []
allWrites (x:xs) = Set.toList $ foldl' (\s (fp,_) -> Set.insert fp s) (Set.fromList $ allWrites xs) $ tWrite x

allReads :: [Trace (FilePath, Hash)] -> [FilePath]
allReads [] = []
allReads (x:xs) = Set.toList $ foldl' (\s (fp,_) -> Set.insert fp s) (Set.fromList $ allReads xs) $ tRead x

changedFiles :: (Trace (FilePath, Hash) -> [(FilePath,Hash)]) -> [Trace (FilePath, Hash)] -> Maybe T -> Set.HashSet FilePath
changedFiles _ _ Nothing = Set.empty
changedFiles _ [] _ = Set.empty
changedFiles f (x:xs) (Just t) = if t == tRun x
                                 then g x xs
                                 else Set.empty
  where g x [] = Set.fromList $ map fst $ f x
        g x (y:ys) = Set.map fst $ Set.difference (Set.fromList $ f x) (Set.fromList $ f y)

changedWrites :: [Trace (FilePath, Hash)] -> Maybe T -> Set.HashSet FilePath
changedWrites = changedFiles tWrite

changedReads :: [Trace (FilePath, Hash)] -> Maybe T -> Set.HashSet FilePath
changedReads = changedFiles tRead

cmdIndex :: (Cmd,[Trace (FilePath, Hash)]) -> [(Cmd,[Trace (FilePath, Hash)])] -> Int
cmdIndex x cmds = fromMaybe (-1) $ elemIndex x cmds

{- Readers are cmds that read something this command wrote // they depend on this command
   writers are cmds that wrote something this command read // i depend on them
   hazards are cmds that wrote after a read or a write
-}
readersWritersHazards :: (Cmd,[Trace (FilePath, Hash)]) -> [(Cmd,[Trace (FilePath, Hash)])] -> [Edge] -> ([Int],[Int],[Int])
readersWritersHazards c cmds =
  foldl' (\(ls1,ls2,ls3) (Edge e1 e2 h) ->
                 if c == e1
                 then let i = cmdIndex e2 cmds in
                        case h of -- check for reader that is not a hazard
                          Nothing -> (i:ls1,ls2,ls3) -- no hazard
                          Just _ -> (ls1,ls2,i:ls3) -- could be writewrite or readwrite; ignore type for now
                 else if c == e2
                      then let i = cmdIndex e1 cmds in
                             case h of -- check for writer that is not a hazard
                               Nothing -> (ls1,i:ls2,ls3) -- no hazard
                               Just _ -> (ls1,ls2,i:ls3) -- could be writewrite or readwrite; ignore type for now
                      else (ls1,ls2,ls3)) -- does not belong to this edge
  ([],[],[])

generateJSON :: Graph -> Maybe T -> String
generateJSON g@Graph{..} t = jsonListLines $ map (showCmdTrace nodes) nodes ++ [showRoot]
  where showCmdTrace cmds cmd@(cmdName,ts) =
          let (readers,writers,hazards) = readersWritersHazards cmd cmds edges
              cw = changedWrites ts t
              built = if null ts then 0 else case t of  -- was this command run in the last run?
                                               Nothing -> 0
                                               (Just t) -> if tRun (head ts) == t then 1 else 0
              changed = if null cw then 0 else 1   -- did the output of this command change in the last run?
              p1 = map (\w -> if Set.member w cw
                              then (w,1)
                              else (w,0)) $ allWrites ts
              p2 = map (\r -> if Set.member r $ changedReads ts t
                              then (r,1)
                              else (r,0)) $ allReads ts in
            jsonList
            [showCmd cmdName
            ,showTime $ firstTTime ts -- max time of all traces
            ,show $ length ts -- number of times traced
            ,show built
            ,show changed
            ,jsonList $ map jsonPair p1  -- all files written during all traces
            ,jsonList $ map jsonPair p2  -- all files read during all traces
            ,show readers -- list of readers with no hazard; depend on me
            ,show writers -- list of writers with no hazard; depend on them
            ,show hazards] -- list of cmds this cmd has a hazard with
        showRoot = jsonList
                   [show "root"
                   ,showTime 0
                   ,show (-1)
                   ,show 1
                   ,show 0
                   ,"[]"
                   ,"[]"
                   ,"[]"
                   ,show $ map (`cmdIndex` nodes) $ graphLeaves nodes edges
                   ,"[]"]
        showTime x = if '.' `elem` y
                     then dropWhileEnd (== '.') $ dropWhileEnd (== '0') y
                     else y
          where y = showDP 4 x


jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
jsonPair (f,i) = "[" ++ show f ++ "," ++ show i ++ "]"
