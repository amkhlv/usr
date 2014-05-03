Setup git server
================

First install `git` :

    aptitude install git

Then all I have to do is to setup the user:

    adduser git

and setup the initial repository:

    su git
    cd
    mkdir myproject.git
    cd myproject.git
    git --bare init

Then, to use it remotely, simply say on my home machine:

    git remote add origin git@server.com:myproject.git

Restricting the user git
========================

Change the login shell for the user `git` to the special shell called `git-shell` :

    chsh -s /usr/bin/git-shell git

Then, when I need _e.g._ to create another project in the future, I will have to
first return to bash: `chsh -s bash git`

