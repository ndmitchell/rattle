
-- | Try replicating the build scripts of fsatrace from:
--   https://github.com/jacereda/fsatrace/blob/master/Makefile
--   (note the two unix.mk and win.mk files)
--
--   We don't deal with the 32bit Windows build since our CI doesn't have a 32bit compiler
module Builder.FSATrace(run) where

import System.Info.Extra
import Development.Rattle
import Development.Shake.FilePath
import Control.Monad


run :: Run ()
run = do
    let plat = if isWindows then "win" else "unix"

    let srcs = ["src/fsatrace.c", "src/"++plat++"/proc.c", "src/"++plat++"/shm.c"] ++
               if isWindows then ["src/win/inject.c","src/win/dbg.c"] else []
    let sosrcs = ["src/unix/fsatraceso.c","src/emit.c","src/unix/shm.c","src/unix/proc.c"]
    let dllsrcs = map ("src/win/" ++) (words "fsatracedll.c inject.c patch.c hooks.c shm.c handle.c utf8.c dbg.c") ++ ["src/emit.c"]

    let cflags = "-g -std=c99 -Wall -O2 -fomit-frame-pointer -fno-stack-protector -MMD"
    let cppflags | isWindows = "-D_WIN32_WINNT=0x600 -isysteminclude/ddk"
                 | otherwise = "-D_GNU_SOURCE -D_DEFAULT_SOURCE=1"

    let ldflags = [] :: [String]
    let ldlibs = if isWindows then "-lntdll -lpsapi" else "-ldl -lrt"
    let ldobjs = ["CRT_noglob.o" | isWindows && False]

    forM_ (srcs ++ if isWindows then dllsrcs else sosrcs) $ \x ->
        cmd "gcc -c" ["-fPIC" | not isWindows] cppflags cflags x "-o" (x -<.> "o")
    let os = map (-<.> "o")

    if isWindows then
        cmd "gcc -shared" ldflags (os dllsrcs) "-o fsatrace64.dll" ldlibs
    else
        cmd "gcc -shared" ldflags (os sosrcs) "-o fsatrace.so" ldlibs

    cmd "gcc" ldflags ldobjs (os srcs) ldlibs "-o" ("fsatrace" <.> exe)
