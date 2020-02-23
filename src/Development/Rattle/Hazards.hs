{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Rattle.Hazards(
    Hazard(..), Recoverable(..),
    HazardSet, mergeHazardSet, newHazardSet, emptyHazardSet, seenHazardSet, addHazardSet,
    recoverableHazard, restartableHazard,
    ) where

import Development.Rattle.Types
import Control.Exception.Extra
import System.Time.Extra
import General.Extra
import Data.List
import Data.Tuple.Extra
import qualified Data.HashMap.Strict as Map
import General.FileName

data ReadOrWrite = Read | Write deriving (Show,Eq)

-- For Write, Seconds is the last possible time at which it was written
-- For Read, Seconds is the earliest possible time at which it was read
-- In both cases, Cmd is the thing that caused the read/write
newtype HazardSet = HazardSet (Map.HashMap FileName (ReadOrWrite, Seconds, Cmd))
    deriving Show


-- | Type of exception thrown if there is a hazard when running the build system.
data Hazard
    = ReadWriteHazard FileName Cmd Cmd Recoverable
    | WriteWriteHazard FileName Cmd Cmd Recoverable
      deriving Show
instance Exception Hazard

data Recoverable = Recoverable | NonRecoverable | Restartable deriving (Show,Eq)

recoverableHazard :: Hazard -> Bool
recoverableHazard WriteWriteHazard{} = False
recoverableHazard (ReadWriteHazard _ _ _ r) = r == Recoverable

restartableHazard :: Hazard -> Bool
restartableHazard (WriteWriteHazard _ _ _ r) = r == Restartable
restartableHazard (ReadWriteHazard _ _ _ r) = r == Restartable

emptyHazardSet :: HazardSet
emptyHazardSet = HazardSet Map.empty

seenHazardSet :: FileName -> HazardSet -> Bool
seenHazardSet x (HazardSet mp) = x `Map.member` mp

newHazardSet :: Seconds -> Seconds -> Cmd -> Touch FileName -> HazardSet
newHazardSet start stop cmd Touch{..} = HazardSet $ Map.fromList $
    map (,(Write,stop ,cmd)) tWrite ++
    map (,(Read ,start,cmd)) tRead

mergeHazardSet :: [Cmd] -> HazardSet -> HazardSet -> ([Hazard], HazardSet)
mergeHazardSet required (HazardSet h1) (HazardSet h2) =
    second HazardSet $ unionWithKeyEithers (mergeFileOps required) h1 h2

-- | addHazardSet a b c d e f == mergeHazardSet a b (newHazardSet c d e f)
addHazardSet :: [Cmd] -> HazardSet -> Seconds -> Seconds -> Cmd -> Touch FileName -> ([Hazard], HazardSet)
addHazardSet required (HazardSet h1) start stop cmd Touch{..} =
    second HazardSet $ insertWithKeyEithers (mergeFileOps required) h1 $
    map (,(Write,stop,cmd)) tWrite ++ map (,(Read,start,cmd)) tRead


-- Very carefully written to include the commands
{- HLINT ignore mergeFileOps "Redundant if" -}
{- HLINT ignore mergeFileOps "Use infix" -}

-- r is required list; s is speculate list
mergeFileOps :: [Cmd] -> FileName -> (ReadOrWrite, Seconds, Cmd) -> (ReadOrWrite, Seconds, Cmd) -> Either Hazard (Maybe (ReadOrWrite, Seconds, Cmd))
mergeFileOps r x (Read, t1, cmd1) (Read, t2, cmd2)
    | t1 <= t2 = Right Nothing -- don't update the Map (an optimisation for the common case)
    | otherwise = Right $ Just (Read, t2, cmd2)
mergeFileOps r x (Write, t1, cmd1) (Write, t2, cmd2) = Left $ WriteWriteHazard x cmd1 cmd2 $
    -- if they both were required, we've got a problem
    if elem cmd1 r && elem cmd2 r then NonRecoverable
    -- if one (or both) were speculated, we might be able to restart and get over it
    else Restartable

mergeFileOps r x (Read, tR, cmdR) (Write, tW, cmdW)
    | tW < tR = -- write happened first
        -- if the write hasn't been demanded but the read has we've
        -- managed to read something that was speculated, which is bad
        if elem cmdR r && notElem cmdW r then hazard Restartable
        -- otherwise, everything is good
        else Right $ Just (Write, tW, cmdW)

    | otherwise = -- read happened first
        -- if the read was speculated, we can ignore it
        if notElem cmdR r then hazard Recoverable
        -- if the write was speculated, we can restart and hopefully it won't recur
        else if notElem cmdW r then hazard Restartable
        -- neither was speculated, but did we use speculation to reorder them?
        -- note the order seems backwards, because r is a snoc-list
        -- FIXME: We might have had them race because of parallelism, so this is optimistically restartable
        else if elemIndex cmdR r < elemIndex cmdW r then hazard Restartable
        -- the user wrote the read before the write
        else hazard NonRecoverable
    where
        hazard = Left . ReadWriteHazard x cmdW cmdR
mergeFileOps r x v1 v2 = mergeFileOps r x v2 v1 -- must be Write/Read, so match the other way around
