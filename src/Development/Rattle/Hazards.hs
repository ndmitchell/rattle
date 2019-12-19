
module Development.Rattle.Hazards(
    Hazard(..), Recoverable(..),
    ReadOrWrite(..),
    HazardSet, mergeHazardSet,
    recoverableHazard, restartableHazard,
    ) where

import Development.Rattle.Types
import Control.Exception.Extra
import General.Extra
import Data.List
import qualified Data.HashMap.Strict as Map


data ReadOrWrite = Read | Write deriving (Show,Eq)

type HazardSet = Map.HashMap FilePath (ReadOrWrite, Timestamp, Cmd)


-- | Type of exception thrown if there is a hazard when running the build system.
data Hazard
    = ReadWriteHazard FilePath Cmd Cmd Recoverable
    | WriteWriteHazard FilePath Cmd Cmd Recoverable
      deriving Show
instance Exception Hazard

data Recoverable = Recoverable | NonRecoverable | Restartable deriving (Show,Eq)

recoverableHazard :: Hazard -> Bool
recoverableHazard WriteWriteHazard{} = False
recoverableHazard (ReadWriteHazard _ _ _ r) = r == Recoverable

restartableHazard :: Hazard -> Bool
restartableHazard (WriteWriteHazard _ _ _ r) = r == Restartable
restartableHazard (ReadWriteHazard _ _ _ r) = r == Restartable



mergeHazardSet :: [Cmd] -> [Cmd] -> HazardSet -> HazardSet -> ([Hazard], HazardSet)
mergeHazardSet required speculate = unionWithKeyEithers (mergeFileOps required speculate)


-- r is required list; s is speculate list
mergeFileOps :: [Cmd] -> [Cmd] -> FilePath -> (ReadOrWrite, Timestamp, Cmd) -> (ReadOrWrite, Timestamp, Cmd) -> Either Hazard (ReadOrWrite, Timestamp, Cmd)
mergeFileOps r s x (Read, t1, cmd1) (Read, t2, cmd2) = Right (Read, min t1 t2, if t1 < t2 then cmd1 else cmd2)
mergeFileOps r s x (Write, t1, cmd1) (Write, t2, cmd2)
  | elem cmd1 r && elem cmd2 r = Left $ WriteWriteHazard x cmd1 cmd2 NonRecoverable
  | otherwise = Left $ WriteWriteHazard x cmd1 cmd2 Restartable -- one write may be an error
mergeFileOps r s x (Read, t1, cmd1) (Write, t2, cmd2)
  | elem cmd1 r && elem cmd2 r && listedBefore cmd1 cmd2
  = Left $ ReadWriteHazard x cmd2 cmd1 NonRecoverable
  | notElem cmd2 r && listedBefore cmd1 cmd2 = Left $ ReadWriteHazard x cmd2 cmd1 Restartable
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
