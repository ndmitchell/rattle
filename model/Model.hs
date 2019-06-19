
module Model(main) where

import Sequential
import Original
import Types
import qualified Data.HashMap.Strict as Map
import Data.Functor.Identity

main :: IO ()
main = let h1 = createState [(Cmd "cmd1" ((T 0),Required) (T 5) [] [] ["a"] ["b"] [])
                            ,(Cmd "cmd2" ((T 1),Required) (T 1) [] [] ["b"] ["a"] [])]
           nh1 = createState [(Cmd "cmd1" ((T 0),Required) (T 5) [] [] ["a"] ["b"] [])
                             ,(Cmd "cmd2" ((T 1),Required) (T 1) [] [] ["b"] ["c"] [])] in
         case seqSched nh1 of
           (Identity st) ->
             case st of
               (State _ _ _ (Left _) _) -> do
                 st <- originalSched st
                 case st of
                   (State _ _ _ (Left done) _) -> putStrLn $ "Tree: " ++ show (fst done)
                   (State _ _ _ (Right h) _) -> putStrLn $ "Hazard after running again " ++ show h
               (State _ _ _ (Right h) _) -> putStrLn $ "hazard: " ++ show h
