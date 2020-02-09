# My Node.js utils

## How to locally publish this (Verdaccio)

    ./node_modules/.bin/tsc
    npm version minor
    npm publish --registry http://localhost:4873

## Exported commands

### Password Hash

    ./node_modules/.bin/amkhlv-hash-pwd  someSALTfdsasdf

--- this produces a hash for use with `crypto.scryptSync(password, salt, 64).toString('hex')`

## Exported functions

### Sleep

    import amkhlv = require('amkhlv-utils')

    await amkhlv.wait(1000)

--- this waits for 1 second.
