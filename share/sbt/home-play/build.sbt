name := """homeplay"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  specs2 % Test,
  "org.commonjava.googlecode.markdown4j" % "markdown4j" % "2.2-cj-1.0",
  "org.mnode.ical4j" % "ical4j" % "1.0.7"
)

scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
  Seq("-doc-root-content", d / "rootdoc.txt" getPath)
}

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
