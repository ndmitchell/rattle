
module Model(main) where

import Sequential
import Speculate
import Original
import Types
import qualified Data.HashMap.Strict as Map

main :: IO ()
main = let h1 = (State [(Cmd "cmd1" t0 (T 5) [] [] ["a"] ["b"] [])
                       ,(Cmd "cmd2" (T 1) (T 1) [] [] ["b"] ["a"] [])]
                  [] [] (E,Map.empty) t0)
           nh1 = (State [(Cmd "cmd1" t0 (T 5) [] [] ["a"] ["b"] [])
                       ,(Cmd "cmd2" (T 1) (T 1) [] [] ["b"] ["c"] [])]
                   [] [] (E,Map.empty) t0) in
         do
           st <- originalSched nh1
           case st of
             Left st -> putStrLn $ "Tree: " ++ (show $ (fst $ done st))
             Right h -> putStrLn "hazard"
