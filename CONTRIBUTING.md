# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also `#akku`.

See the [https://github.com/weinholt/akku/projects](GitHub projects)
page to see the direction of the project.

## Building a release

Building currently requires Chez Scheme and either Akku.scm or manual
installation of dependencies (bundled source releases will be provided
for 0.2.0). Clone the repository:

```
$ git clone https://github.com/weinholt/akku
$ akku install
$ source .akku/bin/activate
$ private/build.chezscheme.sps
```

This produces a tarball in the current directory. The tarball contains
a Petite Chez Scheme distribution, a compiled `akku` program and a
simple installer that creates `~/bin/akku`.

## Submitting patches

Fork the project on GitHub. Please consider using `git commit -s` when
you create patches, to get an automatic sign-off in the commit
message. Please write explanatory commit messages. When you feel
comfortable with your commits, submit a pull request through GitHub.
For larger changes it will be better to discuss the changes ahead of
time, either through a GitHub issue or IRC.

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```
