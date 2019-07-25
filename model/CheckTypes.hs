
module CheckTypes(TState(..), NRHState(..)) where

import Types
import Test.QuickCheck
import qualified Data.HashSet as Set
import Control.Monad.IO.Class
import System.Random

newtype TState = TState State

instance Show TState where
  show (TState st) = show st

instance Arbitrary TState where
  arbitrary = do
    cmds <- sized arbitraryListTCmds
    let pr = map (\c -> c{pos=(fst $ pos c,Speculated)}) cmds
        st = createState cmds
    return $ TState st{prevRun=pr}
        
arbitraryListTCmds :: Int -> Gen [Cmd]
arbitraryListTCmds n = f [] n
  where f ls 0 = return ls
        f ls m = do
          let p = n - m
          c <- arbitraryTCmd ls  -- arbitrary Cmd
          c <- return c{pos=(T p,Required)}
          f (ls ++ [c]) (m - 1)
              
        arbitraryTCmd ls = do
          cmd <- suchThat arbitrary (`notElem` ls) -- get a cmd
          i <- choose ((0,1) :: (Int,Int))
          case i of
            0 -> return cmd{traces=[(rfiles cmd, wfiles cmd)]}
            1 -> do
              rs <- choose (0,20)
              rfiles <- vectorOf rs arbitrary
              ws <- choose (0,20)
              wfiles <- vectorOf ws arbitrary
              return cmd{traces=[(Set.fromList rfiles,Set.fromList wfiles)]}

newtype NRHState = NRHState State

instance Show NRHState where
  show (NRHState st) = show st

instance Arbitrary NRHState where
  arbitrary = do
    cmds <- sized arbitraryListNHCmds
    return $ NRHState (createState cmds)

arbitraryListNHCmds :: Int -> Gen [Cmd]
arbitraryListNHCmds n = do
  r_ <- choose ((1,n-1) :: (Int,Int))
  f r_ Set.empty [] n
  where f h fs ls 0 = return ls
        f h fs ls m = do
          let p = n - m
          c <- suchThat arbitrary (\a -> notElem a ls && not (Set.null (Set.union (rfiles a) (wfiles a))))
          c <- if p == h
               then do let ls2 = Set.toList fs
                       i <- choose ((0,length ls2 - 1) :: (Int,Int))
                       return c{pos=(T p,Required)
                               ,wfiles=Set.insert (ls2 !! i) $ wfiles c}
               else return c{pos=(T p,Required)}
          f h (Set.union (Set.union fs (wfiles c)) (rfiles c)) (ls ++ [c]) $ m - 1          

          
