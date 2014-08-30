With IntelliJ
=============

Should open the project __as an SBT project__ (_i.e._ click on file `build.sbt` when opening)

application.conf
================

application.secret
------------------

How to create a fresh secret key?


    scala> val r = new java.security.SecureRandom
    r: java.security.SecureRandom = java.security.SecureRandom@b4ca6f6

    scala> (1 to 64).map(_=>(r.nextInt(74)+48).toChar).mkString.replaceAll("\\\\+", "/")
    res4: String = cCU`liU?i^R3f:Tk3ekG9a0^hjtwADUi@X2OtjAqKG`vv/>dk@cq_QOnu47WQ<0_

Deployment
==========

Read about it [here](http://www.playframework.com/documentation/2.3.0/Production)

Configuring port etc: read [here](http://www.playframework.com/documentation/2.3.0/ProductionConfiguration)

With `Nginx` : [here](http://www.playframework.com/documentation/2.3.0/HTTPServer)
