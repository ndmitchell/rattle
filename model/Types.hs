{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards  #-}

module Types(Cmd(..), State(..), Hazard(..), Recoverable(..), Action(..)
            , T(..), t0, add, ReadOrWrite(..), Tree(..), TreeOrHazard(..)
            , reduce) where

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.List

data ReadOrWrite = Read | Write deriving (Show,Eq) -- copied from Server.hs

data Cmd = Cmd { id :: String
               , pos :: !T
               , cost :: !T -- predetermined time it takes to "run" this cmd
               , start :: [T]
               , stop :: [T]
               , rfiles :: [String] -- files actually read
               , wfiles :: [String] -- files actually written
               , traces :: [([String],[String])]} -- files recorded read and written
           deriving(Eq) -- arbitrary

instance Show Cmd where
  show Cmd{..} = id

data State = State { toRun :: [Cmd]
                   , prevRun :: [Cmd] -- speculation list in rattle
                   , running :: [Cmd]
                   , done :: (Tree,Map.HashMap FilePath (ReadOrWrite, T, Cmd))
                   , timer :: !T }

data Hazard = ReadWriteHazard FilePath Cmd Cmd Recoverable
            | WriteWriteHazard FilePath Cmd Cmd deriving (Eq)

data Recoverable = Recoverable | NonRecoverable deriving (Show,Eq)

data Action = Done | Start | Finished | Wait

newtype T = T Int -- timestamps
  deriving (Enum,Eq,Ord,Show,Read)

t0 :: T
t0 = T 0

add :: T -> T -> T
add (T i) (T i2) = T (i + i2)

-------------------- tree --------------------------- 

data Tree = Par {pc1 :: Tree
                ,pc2 :: Tree}
          | Seq {c1 :: Tree
                ,c2 :: Tree}
          | L {c :: Cmd}
          | E {}

instance Show Tree where
  show E{} = ""
  show L{..} = show c
  show Seq{..} = "(seq " ++ (show c1) ++ " " ++ (show c2) ++ ")"
  show Par{..} = "(par " ++ (show pc1) ++ " " ++ (show pc2) ++ ")"
  
instance Eq Tree where
  t1 == t2 = f (reduce t1) (reduce t2)
    where f E E = True
          f (L c) (L c2) = c == c2
          f (Seq c1 (Seq c2 c3)) (Seq (Seq c4 c5) c6) = c1 == c4 && c2 == c5 && c3 == c6
          f (Seq c1 c2) (Seq c3 c4) = c1 == c3 && c2 == c4
          f (Par (Par c1 c2) c3) (Par c4 (Par c5 c6)) = (c1 == c4 && c2 == c5 && c3 == c6)
                                                        || (c1 == c4 && c2 == c6 && c3 == c5)
                                                        || (c1 == c5 && c2 == c5 && c3 == c4)
                                                        || (c1 == c5 && c2 == c4 && c3 == c5)
                                                        || (c1 == c6 && c2 == c4 && c3 == c5)
                                                        || (c1 == c6 && c2 == c5 && c3 == c4)
          f (Par c1 c2) (Par c3 c4) = (c1 == c3 && c2 == c4) || (c1 == c4 && c2 == c3)
          f t1 t2 = f t2 t1

instance Semigroup Tree where
  t1 <> t2 = f (reduce t1) (reduce t2)
    where f E t = t
          f t E = t
          f l@L{} l2@L{} = if isBefore l l2
                           then Seq l l2
                           else if isAfter l l2
                                then Seq l2 l
                                else Par l l2
  
          f s@(Seq t1 t2) t3 = if isParallel s t3
                               then Par s t3
                               else if isAfter t2 t3
                                    then Seq (f t1 t3) t2
                                    else Seq t1 (f t2 t3)
          f t1 s@Seq{} = f s t1
          
          f p@(Par t1 t2) t3 = if isParallel p t3 -- parallel w/both
                               then Par p t3
                               else if isAfter t3 p
                                    then Seq p t3
                                    else if isBefore t3 p
                                         then Seq t3 p
                                         else if isParallel t1 t3
                                              then Par t1 (f t2 t3)
                                              else Par (f t1 t3) t2
          f t1 p@Par{} = f p t1

instance Monoid Tree where
  mempty= E

isBefore :: Tree -> Tree -> Bool
isBefore (L c) (L c2) = (stop c) < (start c2)
isBefore (Seq t1 t2) t3 = isBefore t2 t3
isBefore t1 (Seq t2 t3) = isBefore t1 t2
isBefore (Par t1 t2) t3 = isBefore t1 t3 && isBefore t2 t3
isBefore t1 (Par t2 t3) = isBefore t1 t2 && isBefore t1 t3

isAfter :: Tree -> Tree -> Bool
isAfter t1 t2 = isBefore t2 t1

isParallel :: Tree -> Tree -> Bool
isParallel t1 t2 = (not $ isBefore t1 t2) && (not $ isAfter t1 t2)

reduce :: Tree -> Tree
reduce E = E
reduce l@L{} = l
reduce (Seq c1 E) = reduce c1
reduce (Seq E c2) = reduce c2
reduce (Par c1 E) = reduce c1
reduce (Par E c2) = reduce c2
reduce (Seq c1 c2) = Seq (reduce c1) (reduce c2)
reduce (Par c1 c2) = Par (reduce c1) (reduce c2)

  
----------------------- end of tree ------------------------------          

data TreeOrHazard = Tree {t :: Tree
                         ,hs  :: Map.HashMap FilePath (ReadOrWrite, T, Cmd)}
                  | Hazard Hazard

instance Eq TreeOrHazard where
  Tree t1 f1 == Tree t2 f2 = t1 == t2
  Hazard h == Hazard h2 = h == h2
  x == x2 = False

instance Semigroup TreeOrHazard where
  h@(Hazard (WriteWriteHazard _ _ _)) <> _ = h
  _ <> h@(Hazard (WriteWriteHazard _ _ _)) = h
  h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable)) <> _ = h
  _ <> h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable)) = h
  h@(Hazard (ReadWriteHazard _ _ _ Recoverable)) <> _ = h
  _ <> h@(Hazard (ReadWriteHazard _ _ _ Recoverable)) = h
  (Tree t1 f1) <> (Tree t2 f2) = case unionWithKeyEithers mergeFileOps f1 f2 of
                                   (ps@(p:_), _) -> Hazard p
                                   ([], f3) -> Tree (t1 <> t2) f3

instance Monoid TreeOrHazard where
  mempty = Tree E Map.empty


-- copied from rattle
unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e v) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs rhs = foldl' f ([], lhs) $ Map.toList rhs
    where
        f (es, mp) (k, v2) = case Map.lookup k mp of
            Nothing -> (es, Map.insert k v2 mp)
            Just v1 -> case op k v1 v2 of
                Left e -> (e:es, mp)
                Right v -> (es, Map.insert k v mp)

mergeFileOps :: FilePath -> (ReadOrWrite, T, Cmd) -> (ReadOrWrite, T, Cmd) -> Either Hazard (ReadOrWrite, T, Cmd)
mergeFileOps x (Read, t1, cmd1) (Read, t2, cmd2) = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps x (Write, t1, cmd1) (Write, t2, cmd2) = Left $ WriteWriteHazard x cmd1 cmd2
mergeFileOps x (Read, t1, cmd1) (Write, t2, cmd2)
    | listedBefore cmd1 cmd2 = Left $ ReadWriteHazard x cmd2 cmd1 NonRecoverable
    | t1 <= t2 = Left $ ReadWriteHazard x cmd2 cmd1 Recoverable
    | otherwise = Right (Write, t2, cmd2)
  where -- FIXME: listedBefore is O(n) so want to make that partly cached
        listedBefore Cmd{pos=p1} Cmd{pos=p2} = p1 < p2 -- need to fix this still
mergeFileOps x v1 v2 = mergeFileOps x v2 v1 -- must be Write/Read, so match the other way around

