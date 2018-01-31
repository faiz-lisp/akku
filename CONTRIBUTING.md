# Contributing

Want to discuss the project? Chat with `weinholt`
in [`#scheme`](irc://irc.freenode.org/#scheme) on Freenode or open an
issue. There's also `#akku`.

Go to [GitHub projects][projects] to see the direction of the project.

 [projects]: https://github.com/weinholt/akku/projects

## Setting up a development environment

If you already have a working Akku installation (e.g. by installing
one of the released versions) then things are simple:

```
$ git clone https://github.com/weinholt/akku
$ akku install
$ source .akku/bin/activate
```

If your development machine can't run Akku yet then the least manual
way forward is to run Akku on a working machine (e.g. a virtual
machine) and afterwards move the files. Alternatively you can read
Akku.lock and install the projects manually.

## Building a release

Releasing currently requires Chez Scheme and a working development
environment (see above). Run `private/build.chezscheme.sps` and
hopefully it will generate a binary and a source release tarball.

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
