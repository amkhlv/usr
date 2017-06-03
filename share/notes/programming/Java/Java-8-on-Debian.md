OpenJDK from backports
======================

Add the following line to `/etc/apt/sources.list` :

    deb http://http.debian.net/debian jessie-backports main

Then install `openjdk-8-jdk` and:

    update-alternatives --config java
    update-alternatives --config javac


From Oracle
===========

[Instructions here](https://wiki.debian.org/JavaPackage); notice that `make-jpkg` is run as __ordinary user__ (not root).

<span style="color:#ff0000"><b>Attention:</b></span> This installs Java browser plugin! Make sure to disable it!

<span style="color:#ff0000"><b>Run javaws</b></span> (as a regular user) and review the settings in the `security` tab.

<span style="color:#ff0000"><b>Java Policy</b></span> 
