import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.andreimikhailov"
ThisBuild / organizationName := "andreimikhailov"

lazy val root = (project in file("."))
  .settings(
    name := "web",
    libraryDependencies += scalaTest % Test

  )

libraryDependencies += "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.141.59"

libraryDependencies += "org.seleniumhq.selenium" % "htmlunit-driver" % "2.34.0"

libraryDependencies += "org.seleniumhq.selenium" % "selenium-server" % "3.141.59"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1"

libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala" % "2.0.2"

libraryDependencies += "com.fasterxml.jackson" % "jackson-datatype-json-org" % "1.8.0"

// https://mvnrepository.com/artifact/net.liftweb/lift-json
libraryDependencies += "net.liftweb" %% "lift-json" % "3.3.0"

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

libraryDependencies += "com.typesafe" % "config" % "1.3.2"
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

