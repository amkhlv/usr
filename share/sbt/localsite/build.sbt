name := """localsite"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

retrieveManaged := true

scalacOptions ++= Seq("-feature")

javaOptions += "-Dwebdriver.gecko.driver=/usr/local/lib/geckodriver"

libraryDependencies ++= Seq(
  "com.andreimikhailov" % "utils" % "1.0",
  "org.mnode.ical4j" % "ical4j" % "2.0.0",
  "com.vladsch.flexmark" % "flexmark" % "0.9.0",
  "com.vladsch.flexmark" % "flexmark-ext-tables" % "0.9.0",
  "org.scalafx" % "scalafx_2.11" % "8.0.102-R11",
  "org.seleniumhq.selenium" % "selenium-java" % "3.0.1",
  "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.0.1",
  "org.webjars" %% "webjars-play" % "2.5.0",
  "org.webjars" % "jquery" % "3.1.1-1",
  jdbc,
  cache,
  ws,
  filters,
  "org.scalatestplus.play" %% "scalatestplus-play" % "1.5.1" % Test
)

