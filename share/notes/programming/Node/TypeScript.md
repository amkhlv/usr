# Using TypeScript

## Installing TypeScript and ESLint

    npm install --save-dev typescript @types/node ts-node eslint @typescript-eslint/parser @typescript-eslint/eslint-plugin

    node_modules/.bin/tsc --init

    npx eslint --init
    
    npm install --save @types/node



`package.json` would contain scripts:

    "scripts": {
    "lint": "eslint . --ext .ts"
    , "build": "tsc *.ts"
    }


Then, to link: 

    npm run lint

to build:

    npm run build

Have to install types for individual modules, _e.g._ :

    npm install -D selenium-webdriver
    npm install -D @types/selenium-webdriver


## Using ES6

To use `ES6` things like `Map<>` etc need file `tsconfig.json` containing:

    {
      "compilerOptions": {
        "target": "es5",
        "downlevelIteration": true
      }
    }


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
