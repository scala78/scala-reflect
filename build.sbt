ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-reflect",
    libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.13.8",
    libraryDependencies += "org.scalatest" %% "scalatest-funsuite" % "3.2.13" % "test",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % "test"
  )
