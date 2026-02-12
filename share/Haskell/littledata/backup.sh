#!/bin/sh

BACKUP_FILE=littledata.hs.cpio

{
  find app
  echo flake.nix
  echo default.nix
  echo littledata.cabal
} | cpio -ao > $BACKUP_FILE
