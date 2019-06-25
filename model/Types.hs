{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards  #-}

module Types(Cmd(..), State(..), Hazard(..), Recoverable(..), Action(..), Which(..)
            , T(..), t0, add, ReadOrWrite(..), Tree(..), TreeOrHazard(..)
            , reduce, createState, inTree, resetState, Failed(..), inTreeFailed) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.List
import Debug.Trace as Trace

data ReadOrWrite = Read | Write deriving (Show,Eq) -- copied from Server.hs

data Which = Required | Speculated deriving (Eq)

data Cmd = Cmd { id :: String
               , pos :: (T,Which)
               , cost :: !T -- predetermined time it takes to "run" this cmd
               , start :: [T]
               , stop :: [T]
               , rfiles :: Set.HashSet String -- files actually read
               , wfiles :: Set.HashSet String -- files actually written
               , traces :: [(Set.HashSet String, Set.HashSet String)]} -- files recorded read and written
            -- arbitrary

instance Show Cmd where
  show Cmd{..} = id

instance Eq Cmd where
  Cmd{id=id1} == Cmd{id=id2} = id1 == id2 -- I believe this is rattle's standard

instance Hashable Cmd where
  hashWithSalt s Cmd{..} = s `hashWithSalt` (id,cost,start,stop,rfiles,wfiles,traces)

data State = State { toRun :: [Cmd] -- required list in rattle
                   , prevRun :: [Cmd] -- speculation list in rattle
                   , running :: [Cmd]
                   , done :: TreeOrHazard
                   , timer :: !T }

createState :: [Cmd] -> State
createState ls = State ls [] [] (Tree E Map.empty) t0

resetState :: State -> State
resetState st@State{..} = st{done=(Tree E Map.empty), timer=t0}

data Hazard = ReadWriteHazard FilePath Cmd Cmd Recoverable
            | WriteWriteHazard FilePath Cmd Cmd deriving (Show,Eq)

data Recoverable = Recoverable | NonRecoverable deriving (Show,Eq)

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
              ,f :: Failed}
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
  show (L c Yes) = "[" ++ show c ++ "]"
  show (L c No) = show c
  show (Seq cs) = (foldl' (\str c ->  str ++ " " ++ (show c)) "(seq" cs) ++ ")"
  show (Par cs) = (Set.foldl' (\str c -> str ++ " " ++ (show c)) "(par" cs) ++ ")"
 
instance Eq Tree where
  t1 == t2 = f t1 t2
    where f E E = True
          f (L c f1) (L c2 f2) = c == c2 && f1 == f2
          f (Seq cs1) (Seq cs2) = cs1 == cs2
          f (Par cs1) (Par cs2) = cs1 == cs2
          f _ _ = False

instance Semigroup Tree where
  t1_ <> t2_ = f (reduce t1_) (reduce t2_)
    where f E t = t
          f t E = t
          f l@L{} l2@L{} = if isBefore l l2
                           then Seq [l,l2]
                           else if isAfter l l2
                                then Seq [l2, l]
                                else Par $ Set.union (Set.singleton l) (Set.singleton l2)
          f s@(Seq cs) t3 = if isAfter t3 s -- avoid going through cs if unnecessary
                            then Seq $ cs ++ [t3]
                            else helper [] [] cs t3
          f p@(Par ps) t3 = if isBefore t3 p
                            then Seq [t3,p]
                            else if isAfter t3 p
                                 then Seq [p,t3]
                                 else let par = Set.filter (isParallel t3) ps
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
          helper ls [] (x:xs) t1 = if isBefore t1 x -- can stop
                                   then Seq $ ls ++ [t1] ++ (x:xs)
                                   else if isAfter t1 x
                                        then helper (x:ls) [] xs t1
                                        else helper ls [x] xs t1 -- must be parallel
          helper ls ls2 (x:xs) t1 = if isBefore t1 x -- can stop
                                    then Seq $ ls ++ [Par $ Set.union (Set.singleton $ Seq ls2)
                                                      (Set.singleton t1)] ++ (x:xs)
                                    else helper ls (ls2 ++ [x]) xs t1 --  must be parallel
                               
instance Monoid Tree where
  mempty= E

isBefore :: Tree -> Tree -> Bool
isBefore (L c _) (L c2 _) = (stop c) < (start c2)
isBefore (Seq ls) t3 = isBefore (head ls) t3
isBefore t1 (Seq ls) = isBefore t1 (head ls)
isBefore (Par ls) t3 = Set.foldl' (\b t -> b && isBefore t t3) True ls 
isBefore t1 (Par ls) = Set.foldl' (\b t -> b && isBefore t1 t) True ls

isAfter :: Tree -> Tree -> Bool
isAfter t1 t2 = isBefore t2 t1

isParallel :: Tree -> Tree -> Bool
isParallel t1 t2 = (not $ isBefore t1 t2) && (not $ isAfter t1 t2)

inTree :: Cmd -> Tree -> Bool
inTree _ E = False
inTree c (L c1 No) = c == c1
inTree c (L c1 Yes) = False
inTree c (Seq cs) = any (inTree c) cs
inTree c (Par cs) = any (inTree c) $ Set.toList cs

inTreeFailed :: Cmd -> Tree -> Bool
inTreeFailed _ E = False
inTreeFailed c (L c1 Yes) = c == c1
inTreeFailed c (L c1 No) = False
inTreeFailed c (Seq cs) = any (inTreeFailed c) cs
inTreeFailed c (Par cs) = any (inTreeFailed c) $ Set.toList cs

setFailed :: Cmd -> Tree -> Tree
setFailed c (L c1 No) | c == c1 = L c1 Yes
                      | otherwise = L c1 No
setFailed c (Seq cs) = Seq $ map (setFailed c) cs
setFailed c (Par cs) = Par $ Set.map (setFailed c) cs
setFailed _ t = t

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
        reduce_ (Seq [(Par ls)]) = reduce $ Par ls
        reduce_ (Seq [(Seq ls)]) = reduce $ Seq ls
        reduce_ (Seq xs) = Seq $ f xs
        reduce_ (Par s) =  Par $ Set.fromList $ g $ Set.toList s
        f [] = []
        f (x:xs) = case reduce x of
                     E        -> f xs
                     l@L{}    -> l:(f xs)
                     (Seq ls) -> ls ++ (f xs)
                     (Par ls) -> (Par ls):(f xs)

        g [] = []
        g (x:xs) = case reduce x of
                     E        -> g xs
                     l@L{}    -> l:(g xs)
                     (Seq ls) -> (Seq ls):(g xs)
                     (Par ls) -> (Set.toList ls) ++ (g xs)
  
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

instance Semigroup TreeOrHazard where
  h@(Hazard (WriteWriteHazard _ _ _) _) <> _ = h
  _ <> h@(Hazard (WriteWriteHazard _ _ _) _) = h
  h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) <> _ = h
  _ <> h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) = h
  h@(Hazard (ReadWriteHazard _ _ _ Recoverable) _) <> _ = h
  _ <> h@(Hazard (ReadWriteHazard _ _ _ Recoverable) _) = h
  (Tree t1 f1) <> (Tree t2 f2) = case unionWithKeyEithers mergeFileOps f1 f2 of
                                   (ps@(p:_), f3) -> fixup (Hazard (fst p) $ Tree (t1 <> t2) f3)
                                   ([], f3) -> Tree (t1 <> t2) f3
    where fixup (Hazard h@(ReadWriteHazard fp c1 c2 Recoverable) (Tree t hs)) =
            (Hazard h (Tree (setFailed c2 t) (removeFiles c2 hs)))
          fixup x = x -- don't fixup non-recoverable hazards
          removeFiles c@Cmd{..} fs =
            Set.foldl' (\m fp -> case Map.lookup fp m of
                                   Nothing -> m
                                   Just [(_,_,c1)] ->
                                     if c == c1
                                     then  Map.delete fp m
                                     else m
                                   Just xs ->
                                     Map.insert fp (filter (\(_,_,c1) -> not $ c == c1) xs) m)
            fs $ (fst (head traces)) `Set.union` (snd (head traces)) -- if traces are not up to date........

instance Monoid TreeOrHazard where
  mempty = Tree E Map.empty


-- adopted from rattle
unionWithKeyEithers :: (Eq k, Hashable k) => (k -> [v] -> [v] -> Either (a,v) v) -> Map.HashMap k [v] -> Map.HashMap k [v] -> ([(a,v)], Map.HashMap k [v])
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp) -- v2 is a list
            Just v1 -> case op k v1 v2 of
                         Left x@(a,v) -> (x:es, Map.insert k (v:v1) mp) -- fix here to do the insertion as well
                         Right v -> (es, Map.insert k (v:v1) mp)

mergeFileOps :: FilePath -> [(ReadOrWrite, T, Cmd)] -> [(ReadOrWrite, T, Cmd)] -> Either (Hazard,(ReadOrWrite, T, Cmd)) (ReadOrWrite, T, Cmd)
mergeFileOps x ((Read, t1, cmd1):xs) ((Read, t2, cmd2):ys) = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps x ((Write, t1, cmd1):xs) ((Write, t2, cmd2):ys) = Left (WriteWriteHazard x cmd1 cmd2, (Write, max t1 t2, if t1 < t2 then cmd2 else cmd1))
mergeFileOps x ((Read, t1, cmd1):xs) ((Write, t2, cmd2):ys)
    | listedBefore (pos cmd1) (pos cmd2) = Left (ReadWriteHazard x cmd2 cmd1 NonRecoverable, (Write, t2, cmd2))
    | t1 <= t2 = Left (ReadWriteHazard x cmd2 cmd1 Recoverable, (Write, t2, cmd2))
    | otherwise = Right (Write, t2, cmd2)
  where listedBefore (p1,Required) (p2,Required) = p1 < p2
        listedBefore (p1,Speculated) (p2,Speculated) = p1 < p2
        listedBefore (p1,Required) (p2,_) = False -- recoverable
        listedBefore (p1,_) (p2,Required) = False -- shouldn't even need to re-execute in this case 
mergeFileOps x v1 v2 = mergeFileOps x v2 v1 -- must be Write/Read, so match the other way around
