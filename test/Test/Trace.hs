
module Test.Trace(main) where

import Control.Monad
import Data.List.Extra
import Test.Type
import Development.Shake.Command
import System.Directory


check got want = do
    want <- mapM canonicalizePath want
    nubOrd got `intersect` want === want

traceWrite :: [FSATrace] -> [FilePath] -> IO ()
traceWrite ts = check [t | FSAWrite t <- ts]

traceRead :: [FSATrace] -> [FilePath] -> IO ()
traceRead ts = check [t | FSARead t <- ts]


main =
    forM_ [Nothing, Just Shell] $ \opts -> do
        putStrLn "Check GCC works"
        writeFile "helloworld.h" "#include <stdio.h>\n"
        writeFile "helloworld.c" $ unlines
            ["#include \"helloworld.h\""
            ,"int main(){"
            ,"  printf(\"Hello World!\\n\");"
            ,"  return 0;"
            ,"}"
            ]

        xs <- cmd opts "gcc -c helloworld.c"
        traceRead xs ["helloworld.c","helloworld.h"]
        traceWrite xs ["helloworld.o"]

        xs <- cmd opts "gcc helloworld.o -o helloworld.exe"
        traceRead xs ["helloworld.o"]
        traceWrite xs ["helloworld.exe"]

        xs <- cmd opts "helloworld.exe"
        traceRead xs ["helloworld.exe"]

        putStrLn "Check curl works"
        -- Require the Shell below because of https://github.com/jacereda/fsatrace/issues/28
        xs <- cmd opts Shell "curl -sSL http://example.com/ -o example.txt"
        traceWrite xs ["example.txt"]
