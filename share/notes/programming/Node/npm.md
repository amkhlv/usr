# NPM

## Install packages for user, NOT root

Since we installed `Node.js` via [NVM](nvm.html)
all the packages installed via `npm install -g ...` will be
actually installed locally, into some appropriate
location inside `~/.nvm/`

## New project

    mkdir myNewProject
    cd myNewProject
    npm init

## Private registry (Verdaccio)

### Setting multiple registries

In the `~/.npmrc` file, can have:

    @amkhlv:registry=http://localhost:4873
    registry=https://registry.npmjs.org

--- this sets registry to local for the `@amkhlv/` packages, 
and to `https://registry.npmjs.org` for all other. 



