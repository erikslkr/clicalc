ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.0"

lazy val root = (project in file("."))
  .settings(
    name := "clicalc",
    idePackagePrefix := Some("de.erikslkr")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test
