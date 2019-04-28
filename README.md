# Rattle [![Hackage version](https://img.shields.io/hackage/v/rattle.svg?label=Hackage)](https://hackage.haskell.org/package/rattle) [![Stackage version](https://www.stackage.org/package/rattle/badge/nightly?label=Stackage)](https://www.stackage.org/package/rattle) [![Linux build status](https://img.shields.io/travis/ndmitchell/rattle/master.svg?label=Linux%20build)](https://travis-ci.org/ndmitchell/rattle) [![Windows build status](https://img.shields.io/appveyor/ci/ndmitchell/rattle/master.svg?label=Windows%20build)](https://ci.appveyor.com/project/ndmitchell/rattle)

_{Fast, Correct, Simple, Unfinished} - Choose four._

To write a Rattle build system you need to write a script that builds your code. No dependencies, no targets, just a straight-line script that builds your code. Rattle takes that script, and turns it into a build system:

1. Rattle skips commands if their inputs/outputs haven't changed.
2. Rattle copies outputs over if a command has been run before on the same inputs (potentially for a different user).
3. Rattle guesses what commands might be coming next, and speculates of them, to increase parallelism.

The closest build system to Rattle is [Fabrciate](https://github.com/brushtechnology/fabricate), which follows a similar approach, but only provides step 1. Rattle extends that to steps 2 and 3.

### Advantages

Rattle build systems are simple to write - just write commands to build your project. There is only one "Rattle" specific thing, and that's running a command, giving it a tiny API. Rattle build systems can delegate to other build systems quite happily -- if a dependency is built with `make`, just shell out to `make` -- no need to recreate the dependencies with Rattle. You don't need to think about dependencies, and because you aren't writing dependencies, there's no way to get them wrong.

### Disadvantages

The disadvantages of Rattle are that only system commands are skipped, so the zero build time might not be as quick as it could. Rattle also has to guess at useful commands to get parallelism, and that might go wrong.

Rattle fundamentally relies on tracing the files used by system commands, and currently uses the [`fsatrace`](https://github.com/jacereda/fsatrace) program to do so, which must be installed. As a consequence of the design, if `fsatrace` fails, then `rattle` won't work properly. Reasons that `fsatrace` might fail include:

* If you use Go compiled binaries on Linux or Mac then it's unlikely to be traced, see [Issue 24](https://github.com/jacereda/fsatrace/issues/24).
* If you a Mac OS X 1.10 or higher then system binaries that themselves invoke system binaries they won't be traced, unless you [disable System Integrity Protection](https://developer.apple.com/library/content/documentation/Security/Conceptual/System_Integrity_Protection_Guide/ConfiguringSystemIntegrityProtection/ConfiguringSystemIntegrityProtection.html) (which you do at your own risk).
* If you use statically linked binaries on Linux they probably won't be traced.

It is possible to write alternative tracing mechanisms that wouldn't have these limitations, for example:

* [Bigbro](https://github.com/droundy/bigbro/) uses `ptrace` so works for Go binaries and statically linked binaries.
* [traced-fs](https://github.com/jacereda/traced-fs) is an unfinished file system tracer that implements a user file system that would also work for Go binaries and static linking.
