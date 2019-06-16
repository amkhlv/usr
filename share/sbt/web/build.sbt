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

// https://stackoverflow.com/questions/25144484/sbt-assembly-deduplication-found-error/25147568
assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}

val osName: SettingKey[String] = SettingKey[String]("osName")

osName := (System.getProperty("os.name") match {
  case name if name.startsWith("Linux") => "linux"
  case name if name.startsWith("Mac") => "mac"
  case name if name.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
})

libraryDependencies += "org.openjfx" % "javafx-base" % "12.0.1" classifier osName.value

libraryDependencies += "org.openjfx" % "javafx-controls" % "12.0.1" classifier osName.value

libraryDependencies += "org.openjfx" % "javafx-fxml" % "12.0.1" classifier osName.value

libraryDependencies += "org.openjfx" % "javafx-graphics" % "12.0.1" classifier osName.value
