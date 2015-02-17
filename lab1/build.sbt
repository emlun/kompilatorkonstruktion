
val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"

lazy val root = (project in file(".")).
  settings(
    name := "lab1",
    scalaVersion := "2.11.4",
    libraryDependencies += parserCombinators
  )
