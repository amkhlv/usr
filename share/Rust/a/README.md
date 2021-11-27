Installation
============

As root:

    aptitude install libncurses-dev
    aptitude install xsel 

As user:

    mkdir ~/.local/var/
    cargo install --path .

Configuration
=============

First create directory:

    mkdir ~/.local/var

The file `~/a.yaml` should be of the form:

    SectionName:
      SomeName: >
        someprog.sh 
      OtherName: >
        otherprog.sh 
    OtherSection:
      Ation: >
        prog.sh
      ...
    ...



