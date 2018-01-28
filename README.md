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
  and [SPDX license expressions](https://spdx.org/).
* Automatic dependency solver.
* No automatic code execution from packages on installation.
* Verification of downloads and review tools when updating
  dependencies.

Upcoming:

* Easy bundling/compilation of programs along with all dependencies.
* Usable offline (and only going to the Internet after asking for
  permission).

These are big promises and it's a long way to go, but the hope is to
have a modicum of usefulness every step along the way.

## Usage

Download, unpack and run the installer in the latest Akku.scm release
from [GitHub](https://github.com/weinholt/akku/releases). Pre-built
versions are available for Linux amd64 and armhf. (Alternatively clone
the repository and follow the build instructions below, or install it
manually into your Scheme library path). Please verify the OpenPGP
signature.

A vague outline of how things will work:

 - Run `akku init` in a directory to get a suggested Akku.manifest
   printed.
 - Edit the manifest.
 - Run `akku lock` to get a Akku.lock file with all dependencies.
 - Run `akku install` to install the dependencies.
 - Run `source .akku/bin/activate` (in bash) to prepare the
   environment for the programs and libraries in .akku.

The installed libraries and programs should now be available to you,
assuming you use one of these Schemes: Chez Scheme, GNU Guile (with
R6RS settings), Ikarus, Mosh, Racket (plt-r6rs), Sagittarius, Vicare
or Ypsilon.

The install command takes care to place libraries at appropriate
(possibly multiple) locations in the directory tree. It parses all
libraries from the downloaded files and even supports includes.
Downloaded binaries are installed into `.akku/bin`, which is added to
the path by the `activate` script.

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

## Credits

Akku.scm uses the dependency solver from aptitude. The code is
originally copyrighted by Daniel Burrows and ported to Scheme by
Andreas Rottman as port of Dorodongo.

Akku.scm also uses numerous SRFIs and other libraries, all of which
can be found referenced in the Akku.lock file.
