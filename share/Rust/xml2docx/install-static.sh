#!/bin/sh
#
RUSTFLAGS='-C link-arg=-s' cargo install --force --target x86_64-unknown-linux-musl --path .
