#!/usr/bin/env bash

find Cargo.toml src psql-setup.sql | cpio -ao > utils_backup.cpio 



