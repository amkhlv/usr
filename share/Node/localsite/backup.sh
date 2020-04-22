#!/usr/bin/env bash

find index.ts package.json tslint.json start.sh build.sh backup.sh views/  .vscode/ | cpio -ao > localsite.cpio
