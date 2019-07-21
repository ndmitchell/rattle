
module Properties(prop_eq, prop_origEq, prop_consEq, prop_aggrEq
                 ,prop_origEqWData, prop_consEqWData, prop_aggrEqWData
                 ,prop_detectNRH, prop_detectNRHOrig
                 ,prop_detectNRHCons, prop_detectNRHAggr) where


{- Todos: 1. define what it means for 2 trees to be equivalent
          2. define some properties
-}

import Types
import CheckTypes
import Sequential
import Combinations
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class

prop_eq :: State -> Bool
prop_eq st = let Identity st1 = seqSched st
                 Identity st2 = seqSched st in
               st1 `equiv` st2

-- Original scheduler produces a result equivalent to sequential scheduler if it has NO data
prop_origEq :: State -> Property
prop_origEq st = monadicIO $ do
  st1 <- run (case (seqSched st) of
                 Identity st -> return st)
  st2 <- run (originalSched st)
  assert $ st1 `equiv` st2

-- Conservative scheduler produces a result equivalent to sequential scheduler if it has NO data
prop_consEq :: State -> Property
prop_consEq st = monadicIO $ do
  st1 <- run (case (seqSched st) of
                 Identity st -> return st)
  st2 <- run (consSched st)
  assert $ st1 `equiv` st2

-- aggressive scheduler produces a result equivalent to sequential scheduler if it has NO data
prop_aggrEq :: State -> Property
prop_aggrEq st = monadicIO $ do
  st1 <- run (case (seqSched st) of
                Identity st -> return st)
  st2 <- run (aggrSched st)
  assert $ st1 `equiv` st2

{- Original scheduler produces a result equivalent to sequential scheduler
   if it has prev data
-}
prop_origEqWData :: TState -> Property
prop_origEqWData (TState st) = monadicIO $ do
  st1 <- run (case (seqSched st) of
                Identity st -> return st)
  st2 <- run (originalSched st)
  assert $ st1 `equiv` st2
  
{- Conservative scheduler produces a result equivalent to sequential scheduler
   if it has prev data
-}
prop_consEqWData :: TState -> Property
prop_consEqWData (TState st) = monadicIO $ do
  st1 <- run (case (seqSched st) of
                Identity st -> return st)
  st2 <- run (consSched st) -- within
  (assert $ st1 `equiv` st2)


{- Aggressive scheduler produces a result equivalent to sequential scheduler
   if it has prev data
-}
prop_aggrEqWData :: TState -> Property
prop_aggrEqWData (TState st) = monadicIO $ do
  st1 <- run (case (seqSched st) of
                Identity st -> return st)
  st2 <- run (aggrSched st)
  assert $ st1 `equiv` st2
    
{- Sequential scheduler detects all non-recoverable hazards
-}
prop_detectNRH :: NRHState -> Bool
prop_detectNRH (NRHState st) =
  case st of
    (State [] _ _ _ _)  -> True
    (State [x] _ _ _ _) -> True
    st -> let Identity st1 = seqSched st in
            case done st1 of
              (Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) -> True
              (Hazard (WriteWriteHazard _ _ _ NonRecoverable) _)               -> True
              _                                                 -> False

prop_detectNRHOrig :: NRHState -> Property
prop_detectNRHOrig (NRHState st) = monadicIO $ do
  case st of
    (State [] _ _ _ _)  -> assert True
    (State [x] _ _ _ _) -> assert True
    st -> do
      st <- run (originalSched st)
      assert (case done st of
                (Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) -> True
                (Hazard (WriteWriteHazard _ _ _ NonRecoverable) _)               -> True
                _                                                 -> False)

prop_detectNRHCons :: NRHState -> Property
prop_detectNRHCons (NRHState st) = monadicIO $ do
  case st of
    (State [] _ _ _ _)  -> assert True
    (State [x] _ _ _ _) -> assert True
    st -> do
      st <- run (consSched st)
      assert (case done st of
               (Hazard (ReadWriteHazard _ _ _ NonRecoverable) _)  -> True
               (Hazard (WriteWriteHazard _ _ _ NonRecoverable) _) -> True
               _                                                  -> False)

prop_detectNRHAggr :: NRHState -> Property
prop_detectNRHAggr (NRHState st) = monadicIO $ do
  case st of
    (State [] _ _ _ _) -> assert True
    (State [x] _ _ _ _) -> assert True
    st -> do
      st <- run (aggrSched st)
      assert (case done st of
                (Hazard (ReadWriteHazard _ _ _ NonRecoverable) _)  -> True
                (Hazard (WriteWriteHazard _ _ _ NonRecoverable) _) -> True
                _                                                  -> False)


{-  Properties:

 x   1. W/ previous data (both accurate and inaccurate) orig sched produces equiv to seq sched
 x   2. w/ previous data (both accurate and inaccurate) cons sched produces equiv to seq sched
 x   3. Seq scheduler detects all NON-Recoverable Hazards
    4. seq scheduler NEVER encounters Recoverable hazards
    5. cons scheduler detects all non-recoverable hazards
    6. orig scheduler detects all non-recoverable hazards
    7. cons scheduler recovers from all recoverable hazards
    8. orig scheduler recovers from all recoverable hazards
-}



-- calculate work; work is never more than 2x sequential scheduler.... etc


