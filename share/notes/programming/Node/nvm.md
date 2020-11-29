NVM is Node Version Manager
===========================

Install NVM
-----------

As a local user (__not__ root):

    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.2/install.sh | bash

This script appends 3 lines to the very end of `.bashrc`

    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


In particular, the second command sources `~/.nvm/nvm.sh` which prepends to `PATH`



Install Node using NVM
----------------------

    nvm install node
    nvm use node


Update
------

    nvm install node --reinstall-packages-from=node
    
    nvm install-latest-npm

