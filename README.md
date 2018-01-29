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
* Support for compiling native code extensions (given that the user
  permits it).

These are big promises and it's a long way to go, but the hope is to
have a modicum of usefulness every step along the way.

## Dependencies

Akku.scm currently requires the git and curl programs. It has only
been tested on GNU/Linux systems.

## Usage

Download, unpack and run the installer for the latest Akku.scm release
from [GitHub](https://github.com/weinholt/akku/releases). Pre-built
versions are available for GNU/Linux amd64 and armhf. (Alternatively
clone the repository and follow the build instructions below, or
install it manually into your Scheme library path). Please verify the
OpenPGP signature.

A vague outline of how things will work:

 - Run `akku init` in a directory to get a suggested Akku.manifest
   printed. (This is not quite going to give good results right now).
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

## For package authors

Please carry on as you are. There will not be any dogmas or mandates
coming from Akku: it is Akku's job to work with your code. If it works
in a supported Scheme then it should work with Akku. Right now the
majority of R6RS code in git repositories can be used as-is. The
troublesome parts are native code extensions with C code and some
implementation-specific module formats (both of which are planned to
be fixed).

Here are some practices you can try to follow in your life, to make
life easier for everyone else, regardless of package manager:

 - Choose reasonably unique library names whose symbols contain only
   A-Z, a-z, 0-9 and hyphen. Names should be unique even when compared
   case insensitively. Other names are not portable between Schemes
   and operating systems.
 - Use semantic versioning and publish releases of your project:
   https://semver.org/
 - Tag releases in your source code repository (e.g. 1.0.0 is tagged
   with v1.0.0): https://help.github.com/articles/creating-releases/.
   Please use `git tag -s` to sign your tags.
 - Clearly identify the licenses of the code you distribute:
   https://reuse.software/practices/. If no license is declared then
   the default of copyright law applies, which is that no license at
   all has been granted. This makes reuse a potential legal hassle.

## API

Akku.scm uses semantic versioning for its releases, and so needs to
declare an API. The libraries that are part of Akku.scm are currently
not part of the API. The only stable API right now is the command line
interface.

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.
