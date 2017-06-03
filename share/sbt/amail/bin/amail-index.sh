#!/bin/bash

COMMAND="run"

[ "$1" ] && COMMAND="run --lookback $1"

AMAILDIR="$(dirname "$(dirname "$(readlink "$0")")")"

pushd "$AMAILDIR" > /dev/null

pwd

activator "$COMMAND"

popd > /dev/null


