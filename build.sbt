ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "HDMAS-MC"
  )

libraryDependencies += "io.github.uuverifiers" %% "princess" % "2021-11-15"