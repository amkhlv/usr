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



