import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "1.0.0-SNAPSHOT"
ThisBuild / organization     := "com.cberes"
ThisBuild / organizationName := "cberes"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
