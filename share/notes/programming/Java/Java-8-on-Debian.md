Install Java 8 from Oracle
==========================

    echo "deb http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee /etc/apt/sources.list.d/webupd8team-java.list
    echo "deb-src http://ppa.launchpad.net/webupd8team/java/ubuntu trusty main" | tee -a /etc/apt/sources.list.d/webupd8team-java.list
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys EEA14886
    apt-get update

Then start `aptitude` and mark `openjdk-7-...` for purge. This will raise the dependency error, because there
are many programs which depend on `Java`. However, the resolver should offer `oracle-java8-installer` as one of
the ways to satisfy the dependency. Choose it.

<span style="color:#ff0000"><b>Attention:</b></span> This installs Java browser plugin! Make sure to disable it!

<span style="color:#ff0000"><b>Run javaws</b></span> (as a regular user) and review the settings in the `security` tab.
