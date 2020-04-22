# Using TypeScript

## Installing TypeScript and TSLint

    npm install -D typescript ts-node tslint @types/node

    node_modules/.bin/tsc --init

    node_modules/.bin/tslint --init

Have to install types for individual modules, _e.g._ :

    npm install -D selenium-webdriver
    npm install -D @types/selenium-webdriver

## Scripts

### Including commands in packages

`package.json` may contain:

    "bin": {
        "mycommand": "./myCommand.ts"
      },

This will symlink `./node_modules/.bin/mycommand` to `./myCommand.ts`

### Shebang

    #!/bin/sh
    ':' //; exec "$(dirname "$0")/ts-script" "$0" "$@"


(The command `./node_modules/.bin/mycommand` comes from `ts-node`.)
