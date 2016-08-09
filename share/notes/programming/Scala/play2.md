With IntelliJ
=============

Should open the project __as an SBT project__ (_i.e._ click on file `build.sbt` when opening)

which version of Play?
======================

This is determined by:

    addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.0")

in `project/plugins.sbt`

application.conf
================

application.secret
------------------

How to create a fresh secret key?

    sbt play-generate-secret


Deployment
==========

Read about it [here](http://www.playframework.com/documentation/2.3.0/Production)

Configuring port etc: read [here](http://www.playframework.com/documentation/2.3.0/ProductionConfiguration)

With `Nginx` : [here](http://www.playframework.com/documentation/2.3.0/HTTPServer)
