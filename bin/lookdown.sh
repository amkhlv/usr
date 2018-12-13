#!/bin/bash

find ~/a -depth -type l -name 'lookdown.scrbl' |
    while read L
    do
        pushd "$(dirname "$L")"
        bystrotex lookdown
        popd
    done
