
module Original(originalSched) where

import Sequential
import Speculate
import Types
-- rattle's original scheduling policy

originalSched :: State -> IO (Either State Hazard)
originalSched t = do
  st <- speculateSched t
  case st of
    (Right (ReadWriteHazard _ _ _ Recoverable)) -> return $ seqsched t
    x -> return x
