Running in SBT
==============

Go to the root of the project (the folder which contains the file `build.sbt`) and say:

    sbt

At the prompt, say:

    run

To compile the documentation, say:

    doc

The resulting documentation is in `target/scala-2.10/api/`

Also sometimes interesting to run a shell:

    console

More about documentation
========================

It is nice to create the __root documentation__ --- the overview of the package shown in `index.html`

In the root of the project (next to the file `build.sbt`) create the file: `rootdoc.txt`

Then, in `build.sbt` include this line:

    scalacOptions in (Compile, doc) <++= baseDirectory map { d =>
      Seq("-doc-root-content", d / "rootdoc.txt" getPath)
    }

