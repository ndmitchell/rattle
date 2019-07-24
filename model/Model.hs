
module Model(main) where

import Sequential
import Combinations
import Types
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Functor.Identity

main :: IO ()
main = let t1 = createState [(Cmd "cmd1" ((T 0),Required) (T 5) t0 t0 (Set.singleton "a") (Set.singleton "b") []) -- nonRecoverable ReadWrite hazard
                            ,(Cmd "cmd2" ((T 1),Required) (T 1) t0 t0 (Set.singleton "b") (Set.singleton "a") [])]

           t2 = createState [(Cmd "cmd1" ((T 0),Required) (T 5) t0 t0 (Set.singleton "a") (Set.singleton "b") []) -- No hazard (Seq cmd1 cmd2)
                            ,(Cmd "cmd2" ((T 1),Required) (T 1) t0 t0 (Set.singleton "b") (Set.singleton "c") [])]

           t3 = createState [(Cmd "cmd1" ((T 0),Required) (T 10) t0 t0 (Set.singleton "a") (Set.singleton "a") []) -- nonRecoverable WriteWrite hazard
                            ,(Cmd "cmd2" ((T 1),Required) (T 2) t0 t0 (Set.singleton "a") (Set.singleton "a") [])]

           t4 = let st = createState [(Cmd "\130040\970659" ((T 0),Required) (T 10) t0 t0 (Set.singleton "a") (Set.singleton "b") [((Set.singleton "a"),Set.empty)]) -- pretend we've run it before and have outdated info.
                                     ,(Cmd "\12171" ((T 1),Required) (T 1) t0 t0 (Set.singleton "b") (Set.singleton "c") [((Set.singleton "b"),Set.empty)])] in
                  st{prevRun=[(Cmd "cmd1" ((T 0),Speculated) (T 10) t0 t0 (Set.singleton "a") (Set.singleton "b") [((Set.singleton "a"),Set.empty)])
                             ,(Cmd "cmd2" ((T 1),Speculated) (T 1) t0 t0 (Set.singleton "b") (Set.singleton "c") [((Set.singleton "b"),Set.empty)])]} in
         do
           (Identity t1r1) <- return $ seqSched t1
           t1r2 <- originalSched t1
           t1r3 <- consSched t1
           case done t1r1 of
             (Hazard (ReadWriteHazard _ _ _ NonRecoverable) _) ->
               (putStrLn $ "t1: " ++ (show $ (done t1r1 `equiv` done t1r2) && (done t1r2 `equiv` done t1r3)))
             x ->
               putStrLn $ "t1: failed: " ++ show x
           (Identity t2r1) <- return $ seqSched t2
           t2r2 <- originalSched t2
           t2r3 <- consSched t2
           case done t2r1 of
             (Tree _ _) -> putStrLn $ "t2: " ++ (show $ (done t2r1 `equiv` done t2r2) && (done t2r2 `equiv` done t2r3))
             x -> putStrLn $ "t2: failed: " ++ show x
           (Identity t3r1) <- return $ seqSched t3
           t3r2 <- originalSched t3
           t3r3 <- consSched t3
           case done t3r1 of
             (Hazard (WriteWriteHazard _ _ _ _) _) ->
               putStrLn $ "t3: " ++ (show $ (done t3r1 `equiv` done t3r2) && (done t3r2 `equiv` done t3r3))
             x -> putStrLn $ "t3: failed: " ++ show x
           (Identity t4r1) <- return $ seqSched t4
           t4r2 <- originalSched t4
           t4r3 <- consSched t4
           case done t4r1 of
             (Tree _ _) -> (putStrLn $ "t4: " ++ (show $ (done t4r1 `equiv` done t4r2) && (done t4r2 `equiv` done t4r3)))
             x -> putStrLn $ "t4: failed: " ++ show x
