name := """localsite"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

routesGenerator := InjectedRoutesGenerator

scalaVersion := "2.11.11"

retrieveManaged := true

scalacOptions ++= Seq("-feature")

javaOptions += "-Dwebdriver.gecko.driver=/usr/local/lib/geckodriver"

libraryDependencies ++= Seq(
  "com.andreimikhailov" % "utils" % "1.0",
  "org.mnode.ical4j" % "ical4j" % "2.0.0",
  "com.vladsch.flexmark" % "flexmark" % "0.9.0",
  "com.vladsch.flexmark" % "flexmark-ext-tables" % "0.9.0",
  "org.seleniumhq.selenium" % "selenium-java" % "3.0.1",
  "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.0.1",
  "org.webjars" %% "webjars-play" % "2.6.1",
  "org.webjars" % "jquery" % "3.1.1-1",
  "org.scalafx" % "scalafx_2.11" % "8.0.144-R12",
  jdbc,
  cache,
  ws,
  filters,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)


libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
libraryDependencies += "org.eclipse.jetty" % "jetty-server" % "9.4.1.v20170120"
libraryDependencies += "org.scala-lang" % "scala-xml" % "2.11.0-M4"
libraryDependencies += "org.eclipse.jetty" % "jetty-unixsocket" % "9.4.1.v20170120"
libraryDependencies += "com.andreimikhailov" % "utils" % "1.0"
libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.4.14"

libraryDependencies += "org.mnode.ical4j" % "ical4j" % "2.0.0"
libraryDependencies += guice