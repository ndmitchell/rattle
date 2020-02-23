{-# LANGUAGE RecordWildCards #-}

module Benchmark.Redis(main) where

import Benchmark.VsMake
import Development.Shake.Command
import Data.List.Extra
import System.FilePath
import System.Directory


main :: Args -> IO ()
main = vsMake VsMake{..}
    where
        broken = []
        repo = "https://github.com/antirez/redis"
        master = "unstable"
        generateVersion = 1

        generate :: IO String
        generate = do
            cmd_ "make distclean" (EchoStdout False)
            Stdout xs <- cmd "make MALLOC=libc V=1 -j1"
            root <- getCurrentDirectory
            return $ unlines $ fromTrace root $ lines xs

        clean :: IO ()
        clean = cmd_ "make distclean" (EchoStdout False)

        -- should touch .make-prerequisites in src and deps directories to force it to recursively
        -- rebuild stuff
        make = cmd "touch src/.make-prerequisites && touch deps/make-prerequisites && make MALLOC=libc"
        rattle = cmd Shell


fromTrace :: FilePath -> [String] -> [String]
fromTrace root = f []
    where
        f cwds (x:xs)
            -- make[2]: Entering directory '/home/neil/redis-5.0.7/deps'
            | Just (_, x) <- "Entering directory '" `stripInfix` x
            , let cwd = makeRelative root $ takeWhile (/= '\'') x
                = f (cwd:cwds) xs

            -- make[3]: Leaving directory '/home/neil/redis-5.0.7/deps/lua/src'
            | "Leaving directory" `isInfixOf` x = f (tail cwds) xs

            | fst (word1 x) `elem` ["cc","ar"] = (case cwds of c:_ -> "cd " ++ c ++ " && " ++ x; [] -> x) : f cwds xs

            | otherwise = f cwds xs
