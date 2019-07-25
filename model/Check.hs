
module Check(main) where

import Properties
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck (withMaxSuccess 5000 prop_eq)
  quickCheck (withMaxSuccess 5000 prop_origEq)
  quickCheck (withMaxSuccess 5000 prop_consEq)
  quickCheck (withMaxSuccess 5000 prop_aggrEq)
  quickCheck (withMaxSuccess 5000 prop_origEqWData)
  quickCheck (within 100000000 (withMaxSuccess 5000 prop_consEqWData))
  quickCheck (within 100000000 (withMaxSuccess 5000 prop_aggrEqWData))
  quickCheck (withMaxSuccess 5000 prop_detectNRHSeq)
  quickCheck (withMaxSuccess 5000 prop_detectNRHOrig)
  quickCheck (withMaxSuccess 5000 prop_detectNRHCons)
  quickCheck (withMaxSuccess 5000 prop_detectNRHAggr)
  
