#!/bin/bash

AMAILDIR="$(dirname "$(dirname "$(readlink "$0")")")"

pushd "$AMAILDIR" > /dev/null

pwd

cat README.md

popd > /dev/null
