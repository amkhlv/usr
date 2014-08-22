import play.Project._


name := """pile"""

version := "1.0"

libraryDependencies ++= Seq(
  filters,
  "org.webjars" %% "webjars-play" % "2.2.0",
  "org.webjars" % "bootstrap" % "2.3.1",
  "org.commonjava.googlecode.markdown4j" % "markdown4j" % "2.2-cj-1.0",
  "log4j" % "log4j" % "1.2.17",
  "org.mindrot" % "jbcrypt" % "0.3m"
)

scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
  Seq("-doc-root-content", d / "rootdoc.txt" getPath)
}

scalacOptions ++= Seq("-feature")


playScalaSettings
