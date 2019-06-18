
module Original(originalSched) where

import Sequential
import Speculate
import Types
import Data.Functor.Identity
-- rattle's original scheduling policy

-- this scheduler is a combination of 2 schedulers.
-- What to try first; then what to try in case of failure

originalSched :: State -> IO (Either State Hazard)
originalSched t = do
  st <- speculateSched t
  case st of
    (Right (ReadWriteHazard _ _ _ Recoverable)) -> case seqSched t of
                                                     (Identity st) -> return st
    x -> return x
