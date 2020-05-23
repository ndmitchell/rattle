
-- Run commands separated by
-- a ; b - do a, continue with b ignoring the escape code
-- a || b - do a, only do b if a fails
-- a && b - do a, only do b if a succeeds
-- there is no bracketing
module Pipeline(main) where

import Development.Shake.Command
import System.Environment
import System.Exit


main :: IO ()
main = exitWith =<< run =<< getArgs


isSpecial x = x `elem` [";", "&&", "||"]


run :: [String] -> IO ExitCode
run xs | (a, op:b) <- break isSpecial xs = case op of
    ";" -> run a >> run b
    "&&" -> do a <- run a; if a == ExitSuccess then run b else pure a
    "||" -> do a <- run a; if a == ExitSuccess then pure a else run b
run [] = pure ExitSuccess
run (x:xs) = cmd x xs
