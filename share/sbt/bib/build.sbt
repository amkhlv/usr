name := """bib"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.jbibtex" % "jbibtex" % "1.0.15"

libraryDependencies += "org.yaml" % "snakeyaml" % "1.17"

scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
  Seq("-doc-root-content", d / "rootdoc.txt" getPath)

}

