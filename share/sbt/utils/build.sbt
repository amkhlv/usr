name := """utils"""

version := "1.0"

scalaVersion := "2.11.7"

logLevel := Level.Debug

// Change this to another test framework if you prefer
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.mnode.ical4j" % "ical4j" % "2.0.0",
  "com.typesafe.akka" % "akka-actor_2.11" % "2.4.14",
  "org.scalafx" % "scalafx_2.11" % "8.0.102-R11"
)

scalacOptions += "-feature"

lazy val myUtils = project
  .in(file("target/scala-2.11"))
  .settings(
    organization              := "com.andreimikhailov",
    name                      := "utils",
    version                   := "1.0",
    crossPaths                := false,  //don't add scala version to this artifacts in repo
    publishMavenStyle         := true,
    autoScalaLibrary          := false,  //don't attach scala libs as dependencies
    description               := "project for publishing dependency to maven repo, use 'sbt publishLocal' to install it",
    packageBin in Compile     := baseDirectory.value / s"${name.value}_2.11-${version.value}.jar",
    packageDoc in Compile     := baseDirectory.value / s"${name.value}_2.11-${version.value}-javadoc.jar"
  )