ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "fibonacciHeap",
    scalaVersion := "3.3.1"
  )

lazy val junit = "org.junit.jupiter" % "junit-jupiter-api" % "5.9.3"
libraryDependencies += junit % Test
