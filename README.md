# Rattle [![Hackage version](https://img.shields.io/hackage/v/rattle.svg?label=Hackage)](https://hackage.haskell.org/package/rattle) [![Stackage version](https://www.stackage.org/package/rattle/badge/nightly?label=Stackage)](https://www.stackage.org/package/rattle) [![Linux build status](https://img.shields.io/travis/ndmitchell/rattle/master.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/rattle) [![Windows build status](https://img.shields.io/appveyor/ci/ndmitchell/rattle/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/rattle)

A very early prototype of a unique build system.

A forward based build system, where you can make calls to `cmd`. These are:

1) Skipped if their inputs/outputs haven't changed
2) Have their outputs copied in if only the outputs have changed
3) Can be speculated in advance

The innovation over something like Fabricate is that I have a notion
of hazards - if two rules write to the same file, or one writes after
another reads, I consider that a build hazard and throw an error - the
result is no longer consistent. For speculation, I just run commands
that look useful and don't look like they'll cause a hazard. If
speculation does cause a hazard, I give up and try again without
speculation.

This build system fundamentally relies on tracing the files used by system commands, and currently uses the [`fsatrace`](https://github.com/jacereda/fsatrace) program to do so, which must be installed. As a consequence of the design, if `fsatrace` fails, then `rattle` won't work properly. Example reasons that `fsatrace` might fail include:

* If you use Go compiled binaries on Linux or Mac then it's unlikely to be traced, see [Issue 24](https://github.com/jacereda/fsatrace/issues/24).
* If you a Mac OS X 1.10 or higher then system binaries that themselves invoke system binaries they won't be traced, unless you [disable System Integrity Protection](https://developer.apple.com/library/content/documentation/Security/Conceptual/System_Integrity_Protection_Guide/ConfiguringSystemIntegrityProtection/ConfiguringSystemIntegrityProtection.html) (which you do at your own risk).
* If you use statically linked binaries on Linux they probably won't be traced.

It is possible to write alternative tracing mechanisms that wouldn't have these limitations, for example:

* [Bigbro](https://github.com/droundy/bigbro/) uses `ptrace` so works for Go binaries and statically linked binaries.
* [traced-fs](https://github.com/jacereda/traced-fs) is an unfinished file system tracer that implements a user file system that would also work for Go binaries and static linking.
