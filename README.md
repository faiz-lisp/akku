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

## Usage

Download, unpack and run the installer in the latest Akku.scm release
from [GitHub](https://github.com/weinholt/akku/releases). Alternatively
clone the repository and follow the build instructions below.

Currently there is no easy way to add packages. You will need to
manually construct a lockfile. Name it Akku.lock and write something
like this in it:

```
#!r6rs ;; -*-scheme-*-
(import (akku format lockfile))
(projects
 ((name "r6lint")
  (location (git "https://github.com/weinholt/r6lint"))
  (tag "v0.1.0")))    ; alternatively: revision
```

Afterwards you can run `akku install` and all projects will be
downloaded and installed in `.akku/bin` and `.akku/lib`. Lastly you
can run `source .akku/bin/activate` (in bash). The installed libraries
and programs should now be available to you, assuming you use one of
these Schemes: Chez Scheme, GNU Guile (with R6RS settings), Ikarus,
Mosh, Racket, Sagittarius, Vicare or Ypsilon.

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
