#!/bin/bash
set -ex

export PATH=$PWD/bin:$PATH

if [ ! -f bin/akku ]; then
    # When Chez was not available to build the binary.
    ln -s bin/akku.sps bin/akku
fi

tests/test-lockfile1.sh
tests/test-lockfile2.sh

echo All tests passed
