
Opening
=======

To  open the `SBT` project in `IntelliJ` click on the `build.sbt` file.
__Do__ select `use-auto-import` and "Download Sources/Javadoc/Sources for SBT".
Because otherwise it is __very difficult to import manually__ correctly; and `IntelliJ`
ends up not being able to resolve.


Importing JAR files
===================

The root directory, besides the file `build.sbt`, should also contain folders
`project/` and `lib/`. Put your `.jar` file into `lib/`

Then, in `IntelliJ`, go to `File` → `Project Structure` → `Modules` → `Dependencies`
and add that `.jar` file


