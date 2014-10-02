import play.Project._

name := """homeplay"""

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.webjars" %% "webjars-play" % "2.2.0", 
  "org.webjars" % "bootstrap" % "2.3.1",
  "org.commonjava.googlecode.markdown4j" % "markdown4j" % "2.2-cj-1.0",
  "org.mnode.ical4j" % "ical4j" % "1.0.5.2"
)

scalacOptions ++= Seq("-feature")

playScalaSettings

