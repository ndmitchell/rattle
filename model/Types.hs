{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards  #-}

module Types(Cmd(..), State(..), Hazard(..), Recoverable(..), Action(..), Which(..)
            , T(..), t0, add, ReadOrWrite(..), Tree(..), TreeOrHazard(..)
            , reduce, createState, inTree, resetState, Failed(..), inTreeFailed
            , getCmd, Equiv(..), merge, isRestartable, isRecoverable, isNonRecoverable) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.List
import Debug.Trace as Trace
import Test.QuickCheck
import Data.Maybe

class Equiv a where
  equiv :: a -> a -> Bool

data ReadOrWrite = Read | Write deriving (Show,Eq) -- copied from Server.hs

data Which = Required | Speculated deriving (Eq)

data Cmd = Cmd { id :: String
               , pos :: (T,Which)
               , cost :: !T -- predetermined time it takes to "run" this cmd
               , start :: !T
               , stop :: !T
               , rfiles :: Set.HashSet String -- files actually read
               , wfiles :: Set.HashSet String -- files actually written
               , traces :: [(Set.HashSet String, Set.HashSet String)]} -- files recorded read and written
            -- arbitrary

instance Arbitrary Cmd where
  arbitrary = do
    id <- arbitrary
    cost <- suchThat arbitrary (> 0)
    rs <- choose (0,20)
    rfiles <- vectorOf rs arbitrary
    ws <- choose (0,20)
    wfiles <- vectorOf ws arbitrary
    return $ Cmd id (t0,Required) (T cost) t0 t0 (Set.fromList rfiles) (Set.fromList wfiles) []

instance Show Cmd where
  show Cmd{..} = "name: " ++ show id ++
                 "\npos: " ++ show (fst pos) ++
                 "\ncost: " ++  show cost ++
                 "\nrfiles: " ++ show rfiles ++
                 "\nwfiles: " ++ show wfiles ++
                 "\traces: " ++ show traces ++ " }"


instance Eq Cmd where
  Cmd{id=id1} == Cmd{id=id2} = id1 == id2 -- I believe this is rattle's standard

instance Hashable Cmd where
  hashWithSalt s Cmd{..} = s `hashWithSalt` id

data State = State { toRun :: [Cmd] -- required list in rattle
                   , prevRun :: [Cmd] -- speculation list in rattle
                   , running :: Map.HashMap Int Cmd
                   , required :: Int -- index into torun list. 
                   , done :: TreeOrHazard
                   , timer :: !T
                   , limit :: Int }

-- error if not finished
instance Equiv State where
  equiv (State _ _ _ _ t1 _ _) (State _ _ _ _ t2 _ _) = t1 `equiv` t2

instance Show State where
  show (State tr pr rn rq toh t l) = "State: {\n" ++
                                     "ToRun: " ++ show tr ++
                                     "\nprevrun: " ++ show pr ++
                                     "\nrunning: " ++ show rn ++
                                     "\nTreeOrHazard: " ++ show toh ++
                                     "\nTimer: " ++ show t ++
                                     "\nLimit: " ++ show l ++ "}"

instance Arbitrary State where
  arbitrary = do
    cmds <- sized arbitraryListCmds
    return $ createState cmds

arbitraryListCmds :: Int -> Gen [Cmd]
arbitraryListCmds n = f n
  where f :: Int -> Gen [Cmd]
        f 0 = return []   -- 0 through n-1
        f m = do
          let p = n - m
          c <- arbitrary -- arbitrary cmd
          c <- return c{pos=(T p,Required)}
          ls <- f $ m - 1
          return $ c:ls

createState :: [Cmd] -> State
createState ls = State ls [] Map.empty (-1) (Tree E Map.empty) t0 8

resetState :: State -> State
resetState st@State{..} = st{done=Tree E Map.empty
                            , timer=t0}

data Hazard = ReadWriteHazard FilePath Cmd Cmd Recoverable
            | WriteWriteHazard FilePath Cmd Cmd Recoverable deriving (Show,Eq)

data Recoverable = Recoverable | NonRecoverable | Restartable deriving (Show,Eq)

isNonRecoverable :: Hazard -> Bool
isNonRecoverable (WriteWriteHazard _ _ _ NonRecoverable) = True
isNonRecoverable (ReadWriteHazard _ _ _ NonRecoverable) = True
isNonRecoverable _ = False

isRecoverable :: Hazard -> Bool
isRecoverable (WriteWriteHazard _ _ _ Recoverable) = True
isRecoverable (ReadWriteHazard _ _ _ Recoverable) = True
isRecoverable _ = False

isRestartable :: Hazard -> Bool
isRestartable (WriteWriteHazard _ _ _ Restartable) = True
isRestartable (ReadWriteHazard _ _ _ Restartable) = True
isRestartable _ = False

data Action = Done | Start | Finished | Wait

newtype T = T Int -- timestamps
  deriving (Enum,Eq,Ord,Show,Read)

instance Hashable T where
  hashWithSalt s (T i) = hashWithSalt s i

t0 :: T
t0 = T 0

add :: T -> T -> T
add (T i) (T i2) = T (i + i2)
-------------------- tree --------------------------- 

data Tree = Seq [Tree]
          | Par (Set.HashSet Tree)
          | L {c :: Cmd
              ,failed :: Failed}
          | E

data Failed = Yes | No deriving (Eq)

instance Hashable Failed where
  hashWithSalt s Yes = hashWithSalt s "Yes"
  hashWithSalt s No = hashWithSalt s "No"

instance Hashable Tree where
  hashWithSalt s E = hashWithSalt s ""
  hashWithSalt s (L c f) = hashWithSalt s (c,f)
  hashWithSalt s (Seq ls) = hashWithSalt s ls
  hashWithSalt s (Par ls) = hashWithSalt s ls

instance Show Tree where
  show E = ""
  show (L Cmd{..} Yes) = "[" ++ show id ++ "]"
  show (L Cmd{..} No) = show id
  show (Seq cs) = foldl' (\str c ->  str ++ " " ++ show c) "(seq" cs ++ ")"
  show (Par cs) = Set.foldl' (\str c -> str ++ " " ++ show c) "(par" cs ++ ")"
 
instance Eq Tree where
  t1 == t2 = f t1 t2
    where f E E = True
          f (L c f1) (L c2 f2) = c == c2 && f1 == f2
          f (Seq cs1) (Seq cs2) = cs1 == cs2
          f (Par cs1) (Par cs2) = cs1 == cs2
          f _ _ = False

instance Equiv Tree where
  equiv t1 t2 = f (flatten t1) (flatten t2)
    where f Nothing Nothing = True
          f (Just s1) (Just s2) = (Set.size s1 == Set.size s2) && Set.null (Set.difference s1 s2)
          f _ _ = False
          flatten E = Nothing
          flatten (L c1 No) = Just $ Set.singleton c1
          flatten (L c1 Yes) = Nothing
          flatten (Seq cs) = Just $ foldl' Set.union Set.empty (mapMaybe flatten cs)
          flatten (Par cs) = Just $ foldl' Set.union Set.empty (mapMaybe flatten $ Set.toList cs)
          difference s1 s2 = Set.fromList $ g (Set.toList s1) (Set.toList s2)
          g [] ls2 = []
          g ls1 [] = ls1
          g ls1 (x:xs) = g (delete x ls1) xs

instance Semigroup Tree where
  t1_ <> t2_ = f (reduce t1_) (reduce t2_)
    where f E t = t
          f t E = t
          f l@L{} l2@L{}
            | isBefore l l2 = Seq [l,l2]
            | isAfter l l2 = Seq [l2,l]
            | otherwise = Par $ Set.union (Set.singleton l) (Set.singleton l2)
          f s@(Seq cs) t3
            | isAfter t3 s = Seq $ cs ++ [t3]
            | otherwise = helper [] [] cs t3
          f p@(Par ps) t3
            | isBefore t3 p = Seq [t3,p]
            | isAfter t3 p = Seq [p,t3]
            | otherwise = let par = Set.filter (isParallel t3) ps
                              npar = Set.difference ps par in
                            if Set.null npar
                            then Par $ Set.insert t3 ps
                            else if isBefore t3 $ Par npar
                                 then Par $ Set.insert (Seq [t3,Par npar]) par
                                 else Par $ Set.insert (Seq [Par npar, t3]) par
          f t1 t2 = t1 -- f t2 t1

          helper ls [] [] t1 = Seq $ ls ++ [t1]
          helper ls ls2 [] t1 = Seq $ ls ++ [Par $ Set.union (Set.singleton $ Seq ls2)
                                                             (Set.singleton t1)]
          helper ls [] (x:xs) t1
            | isBefore t1 x = Seq $ ls ++ [t1] ++ (x:xs)
            | isAfter t1 x = helper (x:ls) [] xs t1
            | otherwise = helper ls [x] xs t1 -- must be parallel
          helper ls ls2 (x:xs) t1
            | isBefore t1 x = Seq $ ls ++ [Par $ Set.union (Set.singleton $ Seq ls2) -- can stop
                                            (Set.singleton t1)] ++ (x:xs)
            | otherwise = helper ls (ls2 ++ [x]) xs t1 --  must be parallel
                               
instance Monoid Tree where
  mempty= E

isBefore :: Tree -> Tree -> Bool
isBefore (L c _) (L c2 _) = stop c < start c2
isBefore (Seq ls) t3 = isBefore (head ls) t3
isBefore t1 (Seq ls) = isBefore t1 (head ls)
isBefore (Par ls) t3 = Set.foldl' (\b t -> b && isBefore t t3) True ls 
isBefore t1 (Par ls) = Set.foldl' (\b t -> b && isBefore t1 t) True ls

isAfter :: Tree -> Tree -> Bool
isAfter t1 t2 = isBefore t2 t1

isParallel :: Tree -> Tree -> Bool
isParallel t1 t2 = not (isBefore t1 t2) && not (isAfter t1 t2)

getLeaf :: Cmd -> Tree -> Maybe Tree
getLeaf _ E = Nothing
getLeaf c l@(L c1 _) | c == c1 = Just l
                     | otherwise = Nothing
getLeaf c t = case t of
                (Seq cs) -> foldl' f Nothing cs
                (Par cs) -> foldl' f Nothing $ Set.toList cs
  where f Nothing t = getLeaf c t
        f (Just l@(L c1 _)) t = case getLeaf c t of
                                  Nothing            -> Just l
                                  (Just l2@(L c2 _)) -> if start c1 > start c2
                                                        then Just l
                                                        else Just l2

getCmd :: Cmd -> Tree -> Maybe Cmd
getCmd c t = case getLeaf c t of
               Nothing -> Nothing
               (Just (L c1 _)) -> Just c1

inTree :: Cmd -> Tree -> Bool
inTree c t = case getLeaf c t of
               Nothing -> False
               (Just (L _ No)) -> True
               (Just (L _ Yes)) -> False

inTreeFailed :: Cmd -> Tree -> Bool
inTreeFailed c t = case getLeaf c t of
                     Nothing -> False
                     (Just (L _ No)) -> False
                     (Just (L _ Yes)) -> True

setFailed :: Cmd -> Tree -> Tree
setFailed c (L c1 No) | c == c1 = L c1 Yes
                      | otherwise = L c1 No
setFailed c (Seq cs) = Seq $ map (setFailed c) cs
setFailed c (Par cs) = Par $ Set.map (setFailed c) cs
setFailed _ t = t

setAllFailed :: Tree -> Tree
setAllFailed (L c1 No) = L c1 Yes
setAllFailed (Seq cs) = Seq $ map setAllFailed cs
setAllFailed (Par cs) = Par $ Set.map setAllFailed cs
setAllFailed t = t

-- reduce to a fixed point?
reduce :: Tree -> Tree
reduce t = let r = reduce_ t in
             if r == t
             then r
             else reduce r
  where reduce_ E = E
        reduce_ l@L{} = l
        reduce_ (Seq []) = E
        reduce_ (Par ls) | Set.null ls = E
        reduce_ (Seq [Par ls]) = reduce $ Par ls
        reduce_ (Seq [Seq ls]) = reduce $ Seq ls
        reduce_ (Seq xs) = Seq $ f xs
        reduce_ (Par s) =  Par $ Set.fromList $ g $ Set.toList s
        f [] = []
        f (x:xs) = case reduce x of
                     E        -> f xs
                     l@L{}    -> l : f xs
                     (Seq ls) -> ls ++ f xs
                     (Par ls) -> Par ls : f xs

        g [] = []
        g (x:xs) = case reduce x of
                     E        -> g xs
                     l@L{}    -> l : g xs
                     (Seq ls) -> Seq ls : g xs
                     (Par ls) -> Set.toList ls ++ g xs

work :: Tree -> Int
work E = 0
work (L Cmd{..} _) = case cost of
                     (T t) -> t
work (Seq ls) = sum $ map work ls
work (Par s)  = sum $ map work $ Set.toList s

span_ :: Tree -> Int
span_ E = 0
span_ (L Cmd{..} _) = case cost of
                     (T t) -> t
span_ (Seq ls) = sum $ map span_ ls
span_ (Par s) = maximum $ map span_ $ Set.toList s
  
----------------------- end of tree ------------------------------          

data TreeOrHazard = Hazard {h :: Hazard
                           ,t2 :: TreeOrHazard}
                  | Tree {t :: Tree
                         ,hs  :: Map.HashMap FilePath [(ReadOrWrite, T, Cmd)]}

instance Show TreeOrHazard where
  show (Hazard h t) = "Hazard: " ++ show h
  show (Tree t f) = "Tree: " ++ show t

instance Eq TreeOrHazard where
  Tree t1 f1 == Tree t2 f2 = t1 == t2
  Hazard h t1 == Hazard h2 t2 = h == h2 && t1 == t2
  _ == _ = False

instance Equiv TreeOrHazard where
  equiv (Hazard h t1) (Hazard h2 t2) = True -- fix me
  equiv (Tree t1 f1) (Tree t2 f2) = t1 `equiv` t2
  equiv _ _ = False

merge :: [Cmd] -> TreeOrHazard -> TreeOrHazard -> TreeOrHazard
merge _ h@(Hazard (WriteWriteHazard _ _ _ NonRecoverable) _)  _ = h
merge _ _  h@(Hazard (WriteWriteHazard _ _ _ NonRecoverable) _) = h
merge _ h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) _ = h
merge _ _  h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) = h
merge _ h@(Hazard (WriteWriteHazard _ _ _ Restartable) _) _ = h
merge _ _ h@(Hazard (WriteWriteHazard _ _ _ Restartable) _) = h
merge _ h@(Hazard (ReadWriteHazard _ _ _ Restartable) _) _ = h
merge _ _ h@(Hazard (ReadWriteHazard _ _ _ Restartable) _) = h
merge _ h@(Hazard _ _) _ = h
merge _ _ h@(Hazard _ _) = h
merge tr (Tree t1 f1) (Tree t2 f2) =
  let newtree = t1 <> t2 in
    case unionWithKeyEithers (collectHazards tr) f1 f2 of
      (ps@(p:_), f3) -> let worst = getWorst ps in
                    if isNonRecoverable worst
                    then Hazard worst $ Tree newtree f3
                    else if isRestartable worst
                         then fixup (Hazard worst $ Tree newtree f3)
                         else Hazard worst $ fixupAll ps (Tree newtree f3)
      ([], f3) -> Tree newtree f3
  where getWorst [h] = h
        getWorst (h:hs) | isNonRecoverable h = h
                        | otherwise = g h $ getWorst hs
        g h1 h2 | isRestartable h1 && isNonRecoverable h2 = h2
                | isRecoverable h1 && (isNonRecoverable h2 || isRestartable h2) = h2
                | otherwise = h1
        fixupAll [] t = t
        fixupAll (ReadWriteHazard fp c1 c2 Recoverable:hs) (Tree t fs) =
          fixupAll hs (Tree (setFailed c2 t) (removeFiles c2 fs))

        fixup (Hazard h@(ReadWriteHazard fp c1 c2 Recoverable) (Tree t hs)) =
          Hazard h (Tree (setFailed c2 t) (removeFiles c2 hs))
        fixup (Hazard h@(WriteWriteHazard fp c1 c2 Restartable) (Tree t hs)) =
          Hazard h (Tree (setAllFailed t) Map.empty)
        fixup (Hazard h@(ReadWriteHazard fp c1 c2 Restartable) (Tree t hs)) =
          Hazard h (Tree (setAllFailed t) Map.empty)
        fixup x = x -- don't fixup non-recoverable hazards
        removeFiles c@Cmd{..} fs =
          Set.foldl' (\m fp -> case Map.lookup fp m of
                                 Nothing -> m
                                 Just [(_,_,c1)] ->
                                   if c == c1
                                   then  Map.delete fp m
                                   else m
                                 Just xs ->
                                   Map.insert fp (filter (\(_,_,c1) -> c /= c1) xs) m)
          fs $ (uncurry Set.union (head traces))

-- adopted from rattle
unionWithKeyEithers :: (Eq k, Hashable k) => (k -> [v] -> v -> [a]) -> Map.HashMap k [v] -> Map.HashMap k [v] -> ([a], Map.HashMap k [v])
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, [v2]) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k [v2] mp) -- v2 is a list
            Just vs -> case op k vs v2 of
                         [] -> (es, Map.insert k (v2:vs) mp)
                         ls -> (ls ++ es, Map.insert k (v2:vs) mp)


collectHazards :: [Cmd] -> FilePath -> [(ReadOrWrite, T, Cmd)] -> (ReadOrWrite, T, Cmd) -> [Hazard]
collectHazards r x vs v = mapMaybe (`collectHazard` v) vs
  where collectHazard (Read, t1, cmd1) (Read, t2, cmd2) = Nothing
        collectHazard (Write, t1, cmd1) w@(Write, t2, cmd2)
          | cmd1 `elem` r && cmd2 `elem` r = Just $ WriteWriteHazard x cmd1 cmd2 NonRecoverable
          | otherwise = Just $ WriteWriteHazard x cmd1 cmd2 Restartable
        collectHazard (Read, t1, cmd1) w@(Write, t2, cmd2)
          | isRequired cmd1 && isRequired cmd2 && listedBefore cmd1 cmd2
          = Just $ ReadWriteHazard x cmd2 cmd1 NonRecoverable
          | isRequired cmd1 && isRequired cmd2 = Nothing
          | isRequired cmd2 && t1 <= t2 = Just $ ReadWriteHazard x cmd2 cmd1 Recoverable
          | isRequired cmd1 = Just $ ReadWriteHazard x cmd2 cmd1 Restartable
          | otherwise = Nothing
        collectHazard v1 v2 = collectHazard v2 v1
        isRequired c = isJust $ elemIndex c r
        listedBefore c1 c2 = let (Just i1) = elemIndex c1 r
                                 (Just i2) = elemIndex c2 r in
                               i1 < i2
