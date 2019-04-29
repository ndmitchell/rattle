{-# LANGUAGE ScopedTypeVariables, RecordWildCards, TupleSections #-}
{-# OPTIONS_GHC -w #-}

module Development.Rattle.Profile(
  constructGraph, Graph(..), dotStringOfGraph,
  graphData, writeProfile
  ) where

import Development.Rattle.Server
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
data Edge = Edge {end1 :: (Cmd, [Trace Hash])
                 ,end2 :: (Cmd, [Trace Hash])
                 ,hazard :: Maybe Hazard
                 }

data Graph = Graph {nodes :: [(Cmd, [Trace Hash])]
                   ,edges :: [Edge]
                   }

instance Show Edge where
  show (Edge e1 e2 Nothing) = showCmd (fst e1) ++ " -> " ++ showCmd (fst e2)
  show (Edge e1 e2 (Just h)) = showCmd (fst e1) ++ " -> " ++ showCmd (fst e2) ++ " [ label=\"" ++ show h ++ "\" ];"

getCmdsTraces :: RattleOptions -> IO [(Cmd,[Trace Hash])]
getCmdsTraces options@RattleOptions{..} = withShared rattleFiles$ \shared -> do
  cmds <- maybe (return []) (getSpeculate shared) rattleSpeculate
  fmap (takeWhile (not . null . snd)) $ forM cmds $ \x -> (x,) <$> getCmdTraces shared x

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
  writeProfileInternal out graph

writeProfileInternal :: FilePath -> Graph -> IO ()
writeProfileInternal out g = LBS.writeFile out =<< generateHTML g

{- build graph using trace info
   Add an edge between 2 nodes if they both write the same file
   Add an edge between 2 nodes if one reads a file and another writes it -}
createGraph :: [(Cmd,[Trace Hash])] -> Graph
createGraph xs = Graph xs $ g xs
  where g [] = []
        g (x:xs) = let edges = mapMaybe (createEdge x) xs in
          edges ++ g xs

-- assume p1 occurred before p2.
-- find the worst type of hazard if there is an edge
createEdge :: (Cmd,[Trace Hash]) -> (Cmd,[Trace Hash]) -> Maybe Edge
createEdge p1@(cmd1,ts) p2@(cmd2,ls) = -- first look for write write hazard then look for both read/write and write/read hazards
  case writeWriteHazard ts ls of
    Just fp -> Just $ Edge p1 p2 $ Just $ WriteWriteHazard fp cmd1 cmd2
    Nothing -> case readWriteHazard ts ls of
                 Just fp -> Just $ Edge p1 p2 $ Just $ ReadWriteHazard fp cmd1 cmd2
                 Nothing -> -- check for a non hazard edge
                   case readWriteHazard ls ts of
                     Just fp -> Just $ Edge p1 p2 Nothing -- regular edge
                     Nothing -> Nothing -- no edge

-- Is there a writewrite edge?
writeWriteHazard :: [Trace Hash] -> [Trace Hash] -> Maybe FilePath
writeWriteHazard = maybeHazard tWrite

-- Is there a readwrite edge?
readWriteHazard :: [Trace Hash] -> [Trace Hash] -> Maybe FilePath
readWriteHazard = maybeHazard tRead

maybeHazard :: (Trace Hash -> [(FilePath, Hash)]) -> [Trace Hash] -> [Trace Hash] -> Maybe FilePath
maybeHazard _ [] ls = Nothing
maybeHazard _ ls [] = Nothing
maybeHazard f (t:ts) ls =
  case find (\y -> isJust $ memberWrites y ls) $ f t of
    Just (fp,_) -> Just fp
    Nothing -> maybeHazard f ts ls

memberWrites :: (FilePath, Hash) -> [Trace Hash] -> Maybe FilePath
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
showCmd (Cmd _ exe args) = show $ showCmdHelper $ exe : args

showCmdHelper :: [String] -> String
showCmdHelper [] = ""
showCmdHelper (x:xs) = x ++ " " ++ showCmdHelper xs

dotStringOfGraph :: RattleOptions -> IO String
dotStringOfGraph options = do
  edges <- constructGraph options
  generateDotString edges

{-
predictParallelism :: RattleOptions -> IO [[Cmd]]
predictParallelism  options = do
  edges <- constructGraph options
  return $ calculateParallelism edges


calculateParallelism :: [Edge] -> [[Cmd]]
calculateParallelism xs = calculateParallelismDriver xs HashMap.empty

{- what is the best way to present this? -}
calculateParallelismDriver :: [Edge] -> Map Cmd () -> [[Cmd]]
calculateParallelismDriver [] m =
calculateParallelismDriver (e:es) m =
  -- for each edge e, with end1 and end2.
  -- for end2 ; say new level is level(end1) + 1;
  -- set level of end2 to new level if current level is less than new level
  let c1 = fst $ end1 e
      c2 = fst $ end2 e in
    let (v, nm) = case HashMap.lookup c1 m of
                    Just v  -> (v, m)
                    Nothing -> (1, HashMap.insert c1 1 m) in -- add c1 at a level of 1.
      case HashMap.lookup c2 m of
        Just v2 -> if (v + 1) > v2
                   then calculateParallelismDriver es $ HashMap.insert c2 (v + 1) m
                   else calculateParallelismDriver es m
        Nothing -> calculateParallelismDriver es $ HashMap.insert c2 (v + 1) m

-}

graphRoots :: [(Cmd,[Trace Hash])] -> [Edge] -> [(Cmd,[Trace Hash])]
graphRoots = foldr (delete . end2)

maxTTime :: [Trace Hash] -> Seconds
maxTTime xs = maximum $ 0 : map tTime xs

work :: Graph -> Seconds
work (Graph ns es) = sum $ map (maxTTime . snd) ns

spanGraph :: Graph -> Seconds
spanGraph (Graph ns es) =
  -- get roots and calculate span for each root; take max
  -- roots are the cmds that are only end1;
  -- probably want a hashset from cmd to edges
  let cmds = foldl' (\m (Edge e1 e2 h) -> Map.insertWith (++) e1 [e2] m) Map.empty es
      roots = graphRoots ns es in
    foldl' (\m c -> max m $ spanCmd c cmds) 0.0 roots

spanCmd :: (Cmd, [Trace Hash]) -> Map.HashMap (Cmd, [Trace Hash]) [(Cmd,[Trace Hash])] -> Seconds
spanCmd cmd@(c,ts) cmds =
  case Map.lookup cmd cmds of
    Nothing -> maxTTime ts
    Just ls -> maxTTime ts + foldl (\m c -> max m $ spanCmd c cmds) 0.0 ls

parallelism :: Graph -> Seconds
parallelism g = work g / spanGraph g

generateHTML :: Graph -> IO LBS.ByteString
generateHTML xs = do
  report <- readDataFileHTML "profile.html"
  let f "data/profile-data.js" = return $ LBS.pack $ "var profile =\n" ++ generateJSON xs
  runTemplate f report

allWrites :: [Trace Hash] -> [FilePath]
allWrites [] = []
allWrites (x:xs) = Set.toList $ foldl' (\s (fp,_) -> Set.insert fp s) (Set.fromList $ allWrites xs) $ tWrite x

allReads :: [Trace Hash] -> [FilePath]
allReads [] = []
allReads (x:xs) = Set.toList $ foldl' (\s (fp,_) -> Set.insert fp s) (Set.fromList $ allReads xs) $ tRead x

cmdIndex :: (Cmd,[Trace Hash]) -> [(Cmd,[Trace Hash])] -> Int
cmdIndex x cmds = fromMaybe (-1) $ elemIndex x cmds

{- Readers are cmds that read something this command wrote // they depend on this command
   writers are cmds that wrote something this command read // i depend on them
   hazards are cmds that wrote after a read or a write
-}
readersWritersHazards :: (Cmd,[Trace Hash]) -> [(Cmd,[Trace Hash])] -> [Edge] -> ([Int],[Int],[Int])
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

generateJSON :: Graph -> String
generateJSON Graph{..} = jsonListLines $ map (showCmdTrace nodes) nodes
  where showCmdTrace cmds cmd@(cmdName,ts) =
          let (readers,writers,hazards) = readersWritersHazards cmd cmds edges in
            jsonList
            [showCmd cmdName
            ,showTime $ maxTTime ts -- max time of all traces
            ,show $ length ts -- number of times traced
            ,show $ allWrites ts -- all files written during all traces
            ,show $ allReads ts -- all files read during all traces
            ,show readers -- list of readers with no hazard; depend on me
            ,show writers -- list of writers with no hazard; depend on them
            ,show hazards] -- list of cmds this cmd has a hazrd with
        showTime x = if '.' `elem` y
                     then dropWhileEnd (== '.') $ dropWhileEnd (== '0') y
                     else y
          where y = showDP 4 x


jsonListLines xs = "[" ++ intercalate "\n," xs ++ "\n]"
jsonList xs = "[" ++ intercalate "," xs ++ "]"
