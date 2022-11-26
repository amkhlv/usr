#!/usr/bin/env bash

{ echo README.md 
  echo Cargo.toml 
  find src 
} | cpio -ao > rem.cpio

