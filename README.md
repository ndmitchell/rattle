# Rattle [![Hackage version](https://img.shields.io/hackage/v/rattle.svg?label=Hackage)](https://hackage.haskell.org/package/rattle) [![Stackage version](https://www.stackage.org/package/rattle/badge/nightly?label=Stackage)](https://www.stackage.org/package/rattle) [![Build status](https://img.shields.io/github/workflow/status/ndmitchell/rattle/ci.svg)](https://github.com/ndmitchell/rattle/actions)

_{Fast, Correct, Simple, Unfinished} - Choose four._

To write a Rattle build system you need to write a script that builds your code. No dependencies, no targets, just a straight-line script that builds your code. Rattle takes that script, and turns it into a build system:

1. Rattle skips commands if their inputs/outputs haven't changed.
2. Rattle copies outputs over if a command has been run before on the same inputs (potentially for a different user).
3. Rattle guesses what commands might be coming next, and speculates of them, to increase parallelism.

The closest build system to Rattle is [Fabricate](https://github.com/brushtechnology/fabricate), which follows a similar approach, but only provides step 1. Rattle extends that to steps 2 and 3.

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
* [BuildXL](https://github.com/Microsoft/BuildXL/blob/master/Documentation/Specs/Sandboxing.md#macos-sandboxing) uses a Mac sandbox based on KAuth + TrustedBSD Mandatory Access Control (MAC).

### Relationship to Shake

Rattle was designed by the same author as the [Shake build system](https://shakebuild.com/). Rattle is named as [a follow-on](https://en.wikipedia.org/wiki/Shake,_Rattle_and_Roll) from Shake, but not a replacement -- Shake continues to be actively developed and is a much more mature system.

# Rattle User Manual / Paper

_The rest of this README serves as a dumping ground for things that will either go into a user manual or an academic paper (if one ever gets written). As a result, it is a bit of a mismatch, and on top of that it's very definitely not complete._

## Abstract

Most build systems are _backwards_ build systems, users define rules to produce targets. These systems have a lot of advantages -- they encode parallelism, they are compositional and the user can easily build any subtree of dependencies. However, they require manually specifying dependencies and all the state of an object must be encoded in its filename. The alternative, and poorly studied, _forwards_ build systems are structured very differently, which we argue produces simpler systems for the end user, at least at small to medium scale.

## Introduction

In a backwards build system (e.g. Shake, Bazel, Make) a rule to build a C file will probably look something like:

```
"*.o" %> \out -> do
    let src = out -<.> "c"
    need [src]
    cmd "gcc -c" src "-o" out
```

Taking a look at this rule, it has 4 lines:

1. First line, say how to build `.o` files.
2. Compute, from the `.o` filename alone, the `.c` source it corresponds to.
3. Add an explicit dependency on the `.c` file.
4. Specify the command and run it.

Of these steps, only step 4 is truely building things, the other steps are about providing the metadata for a backwards build system to operate. And furthermore, in a practical build system, we're likely to have to go further -- encoding optimisation level in the `.o` file, and depending on any headers that were used.

To describe it as a backwards build systems is apt. In all likelihood we will have had a `.c` file in mind, then we generate the `.o` file, then we go back to the `.c` file -- all a little odd. In contrast, a forward build system can be expressed more directly:

```
buildCSrc src = cmd "gcc -c" src "-o" (src -<.> "o")
```

We can talk about the `.c` file directly, so don't need to extra information back from the output. We don't need to add dependencies. We just specify the command line. Simple.

In the rest of this paper we describe how to construct a forward build system which is as powerful as a modern fully featured backward build system. There are trade offs, particularly at scale, which we describe and in some cases mitigate.

## The no-op forward build system

We believe forward build systems should be simple, so let's take the other extreme, and think about what a build system is in it's minimality. At an absolute minimum, it must list the commands to be run. For example, to build a C executable we might say:

```
gcc -c hello.c -o hello.o
gcc hello.o -o hello.exe
```

However, we've actually gone slightly further than just listing the commands, but supplied them in an order which satisfies the dependencies. We haven't mentioned the dependencies, but we have written the link command after the compile, which is an order that does satisfy them.

Some build systems are able to build without any ordering, by restarting and retrying commands. While an interesting area of research, we don't persue that path, but see David Roundry's system.

So, given this straight-line code that builds the program what are we missing relative to a build system?

1. Build systems don't rebuild commands whose inputs haven't changed.
2. Some build systems are able to download results from a shared store (e.g. cloud build systems).
3. Backwards build systems can extract parallelism.

In the next three sections we add back each of these aspects.

## Avoid rebuilds of things that haven't changed

Given a command, if you know what files it reads and writes, and none of those files have changed, you can skip running it providing that if it were deterministic it wouldn't be harmful. As an example, if the compiler produces a different object file each time, but it would be OK with not doing that, then you're fine. In practice this means that any command that isn't relied on to produce fresh entropy (the time, a GUID, a random number) is fine to skip subsequent times. Those commands that do produce fresh entropy are not well suited to a build system anyway, so aren't typically used in build systems.

This got solved by fabricate. You can use a system access tracer to watch what a process reads/writes, and have a database storing the command, previous reads/writes, and then skip it if the values haven't changed. This step is not complex.

However, it a build writes a file _twice_ in a run, or reads it then writes to it, the result will be that a subsequent rebuild will have stuff to do, as it won't end at a quiescent state. To detect that, we introduce the concept of hazards.

## Shared build cache

Given knowledge of the reads/writes of a command, it's not too hard to store them in a cloud, and satisfy commands not by rerunning them, but by copying their result from a shared cache. While this works beautifully in theory, in practice it leads to at least three separate problems which we solve.

### Relative build dirs

Often the current directory, or users profile directory, will be accessed by commands. These change either if the user has two working directories, or if they use different machines. We solve this by having a substitution table.

### Compound commands

Sometimes a command will produce something that is user specific, but the next step will make it not user specific. To fix that we add compound commands. You can do it with a shell script, using `pipeline`, or with TemplateHaskell blocks.

### Non-deterministic builds

We solve this by having `.rattle.hash` files which we substitute for reading.

## Generating parallelism

We use speculation and hazards to figure out if speculation went wrong.

## Relative simplicity

Compare Rattle to Shake. The interface to Rattle is basically "run this command", providing a vastly simpler API. In contast, Shake has a lot of API to learn and use.
