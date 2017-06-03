name := """amail"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "com.typesafe" % "config" % "1.3.1"

libraryDependencies += "org.apache.james" % "apache-mime4j" % "0.8.0"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.2.0"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.16.1"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.6.4"

libraryDependencies += "javax.mail" % "mail" % "1.4.7"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"


