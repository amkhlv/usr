import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "amail2",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.typesafe" % "config" % "1.3.4",
    libraryDependencies += "org.apache.james" % "apache-mime4j" % "0.8.3",
    libraryDependencies += "com.typesafe.slick" % "slick_2.12" % "3.3.2",
    libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.28.0",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.26",
    libraryDependencies += "javax.mail" % "mail" % "1.4.7"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
