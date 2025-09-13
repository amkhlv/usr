#!/bin/sh

BACKUP_FILE=inkint.cpio

{
  find app
  find . -maxdepth 1 -type f -not -name $BACKUP_FILE -not -name flake.lock 
} | cpio -ao > $BACKUP_FILE
