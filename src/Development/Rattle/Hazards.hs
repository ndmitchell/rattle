
module Development.Rattle.Hazards(
    Hazard(..), Recoverable(..),
    recoverableHazard, restartableHazard,
    ) where

import Development.Rattle.Types
import Control.Exception.Extra


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
