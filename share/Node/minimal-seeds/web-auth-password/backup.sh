#!/usr/bin/env bash

find index.ts package.json backup.sh views/  .vscode/ | cpio -ao > web-auth-password.cpio
