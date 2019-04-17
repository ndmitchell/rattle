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
