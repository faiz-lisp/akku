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
* Easy bundling/compilation of programs along with all dependencies.
* Usable offline (and only going to the Internet after asking for
  permission).
* Support for compiling native code extensions (given that the user
  permits it).

These are big promises and it's a long way to go, but the hope is to
have a modicum of usefulness every step along the way.

## Dependencies

Akku.scm currently requires the git and curl programs. It has only
been tested on GNU/Linux systems. Assistance in porting is very
welcome.

## Installation

There are two options:

 - Download, unpack and run the binary installer
   from [GitHub](https://github.com/weinholt/akku/releases). Pre-built
   versions are available for GNU/Linux amd64. The installation is
   completely contained to `~/.akku`.

 - Download the source bundle
   from [GitHub](https://github.com/weinholt/akku/releases) (files
   ending with `.src.tar.xz`). This version is a little slower to
   start, because it is recompiled each time, but it may run on more
   types of systems. It requires Chez Scheme 9.5 or later.

Please remember to verify the OpenPGP signatures. The releases are
signed with [E33E61A2E9B8C3A2][key].

 [key]: https://pgp.surfnet.nl/pks/lookup?op=vindex&fingerprint=on&search=0xE33E61A2E9B8C3A2

## Usage

Here's a high level view of how to get started:

 - Create a new directory for your project or go to your existing
   project directory. This step is important.
 - Run `akku list` to list the packages.
 - Run `akku install <pkg>` to install a named package. This also
   installs the code in your current directory into `.akku`.
 - Run `source .akku/bin/activate` (in bash) to prepare the
   environment for the programs and libraries in `.akku`.

The installed libraries and programs should now be available to you,
assuming you use one of these Schemes: Chez Scheme, GNU Guile (with
R6RS settings), Ikarus, Mosh, Racket (plt-r6rs), Sagittarius, Vicare
or Ypsilon.

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

## Other package managers for Scheme

Akku.scm is not the only option. Here are a some to compare with:

 - For R7RS libraries there is [Snow2](http://snow-fort.org).
 - Closely related
   is [snow2-client](https://github.com/sethalves/snow2-client),
   based on a proposed repository format for R7RS packages.
 - Racket has its own
   successful [Racket package system](http://pkgs.racket-lang.org/).
 - Chicken is famous for its [eggs](http://wiki.call-cc.org/eggs).
 - Gambit has its [Spheres](http://www.schemespheres.org/).

There are even other R6RS package managers:

 - [Dorodango](https://gitlab.com/rotty/dorodango) by Andreas
   Rottmann. Akku's dependency solver is nicked from Dorodango.
   It works more like traditional package managers, like APT.
 - [Raven](http://ravensc.com/) is a fairly new package manager aimed
   at Chez Scheme.

## License

Akku.scm is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.
