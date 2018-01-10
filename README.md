# Akku.scm

[![Build Status](https://travis-ci.org/weinholt/akku.svg?branch=master)](https://travis-ci.org/weinholt/akku)

> "The purpose of the machine is to make drudgery unnecessary."
>
> &mdash; Alan Watts

Akku.scm is a language package manager for Scheme. It grabs hold of
code and vigorously shakes it until it behaves properly.

It is quite early in development. The goal is to have a package
manager with these properties, in no particular order:

* Use libraries from R6RS projects without special preparation.
* Automatically prepare code for use in any R6RS implementation,
  perhaps even non-R6RS code (or non-R6RS implementations).
* "Two file" system.
    * The manifest defines packages and their direct dependencies.
    * The lockfile defines all transitive dependencies and where to
      download them, giving repeatable results.
* One simple command to download everything needed for a project.
* Usable both with decentralized projects and central package
  repositories.
* Use modern standards for package management, such
  as [Semantic Versions (SemVer)](http://semver.org/)
  and [SPDX license identifiers](https://spdx.org/).
* Easy bundling/compilation of programs along with all dependencies.
* Usable offline (and only going to the Internet after asking for
  permission).
* No automatic code execution from packages on installation.
* Verification of downloads and review tools when updating
  dependencies.
* Automatic dependency solver.

These are big promises and it's a long way to go, but the hope is to
have a modicum of usefulness every step along the way.

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue.

## Usage

Download, unpack and run the installer in the latest Akku.scm release
from [GitHub](https://github.com/weinholt/akku/releases). Pre-built
versions are available for Linux amd64 and armhf. (Alternatively clone
the repository and follow the build instructions below, or install it
manually into your Scheme library path).

Currently there is no easy way to add packages, since only the
lockfile part of the package manager has been implemented. You will
need to manually construct a lockfile. Name it Akku.lock and write
something like this:

```
#!r6rs ;; -*-scheme-*-
(import (akku format lockfile))
(projects
 ((name "chez-srfi")
  (location (git "https://github.com/akeep/chez-srfi.git"))
  (revision "42d6d4cf4f506ce41152f16e30e0d7e059faef95"))
 ((name "hashing")
  (location (git "https://github.com/weinholt/hashing.git"))
  (revision "de6aa096166bb026eb927b56f763707180ec0330"))
 ((name "struct-pack")
  (location (git "https://github.com/weinholt/struct-pack.git"))
  (revision "eec155b069d03c12be62444754514a8fff34d4ea"))
 ((name "xitomatl")
  (location (git "https://github.com/weinholt/xitomatl.git"))
  (revision "b9babf030c13d9fc81fe1482aa49ff0b8fa0fee6")))
```

Afterwards you can run `akku install` and all projects will be
downloaded and installed in `.akku/bin` and `.akku/lib`. Lastly you
can run `source .akku/bin/activate` (in bash). The installed libraries
and programs should now be available to you, assuming you use one of
these Schemes: Chez Scheme, GNU Guile (with R6RS settings), Ikarus,
Mosh, Racket (plt-r6rs), Sagittarius, Vicare or Ypsilon.

The install command takes care to place libraries at appropriate
(possibly multiple) locations in the directory tree. It parses out all
libraries from the downloaded files and even supports includes.
Downloaded binaries are installed into `.akku/bin`, which is added to
the path by the `activate` script.

## Building a release

Building currently requires Chez Scheme. Clone the repository:

```
$ git clone https://github.com/weinholt/akku
$ private/build.chezscheme.sps
```

This produces a tarball in the current directory. The tarball contains
a Petite Chez Scheme distribution, a compiled `akku` program and a
simple installer that creates `~/bin/akku`.

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.
