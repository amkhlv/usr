name := "bib"

version := "1.0"


libraryDependencies += "org.jbibtex" % "jbibtex" % "1.0.14"

libraryDependencies += "org.yaml" % "snakeyaml" % "1.13"

scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
  Seq("-doc-root-content", d / "rootdoc.txt" getPath)
}