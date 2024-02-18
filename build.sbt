ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "WordleSolver"
  )
