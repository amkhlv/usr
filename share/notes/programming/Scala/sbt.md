Installing from repository
==========================

    apt-get install apt-transport-https

    echo "deb https://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list
    apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823
    apt-get update
    apt-get install sbt



Installing manually
===================

Get `sbt-launch.jar` from [here](https://dl.bintray.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.13/jars/)
and save it into `/usr/local/lib` ; then `sbt` will be the following script:

    #!/bin/bash
    SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
    java $SBT_OPTS -jar /usr/local/lib/sbt-launch.jar "$@"

