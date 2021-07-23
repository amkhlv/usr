#!/usr/bin/env bash

tr '\n' '\0' | xargs -0 urlencode -d | tr '\n' '\0'
