#!/usr/bin/env bash

sed -e 's/\.pdf$/.pdq/g' | tr '\n' '\0' | xargs -0 urlencode -d | tr '\n' '\0' | xargs -0 xdg-open
