{-# LANGUAGE RecordWildCards #-}

module Tree(Tree.insert) where

import Types
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.List as List

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
  

insertTree :: Tree -> Tree -> Tree
insertTree t1 t2 = f (reduce t1) (reduce t2)
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

insert :: TreeOrHazard -> TreeOrHazard -> TreeOrHazard
insert h@(Hazard (WriteWriteHazard _ _ _)) _ = h
insert _ h@(Hazard (WriteWriteHazard _ _ _)) = h
insert h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable)) _ = h
insert _ h@(Hazard (ReadWriteHazard _ _ _ NonRecoverable)) = h
insert h@(Hazard (ReadWriteHazard _ _ _ Recoverable)) _ = h
insert _ h@(Hazard (ReadWriteHazard _ _ _ Recoverable)) = h
insert (Tree t1 f1) (Tree t2 f2) = case unionWithKeyEithers mergeFileOps f1 f2 of
                                     (ps@(p:_), _) -> Hazard p
                                     ([], hazards2) -> Tree (insertTree t1 t2) hazards2

-- copied from rattle
unionWithKeyEithers :: (Eq k, Hashable k) => (k -> v -> v -> Either e v) -> Map.HashMap k v -> Map.HashMap k v -> ([e], Map.HashMap k v)
unionWithKeyEithers op lhs rhs = List.foldl' f ([], lhs) $ Map.toList rhs
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
                


  
