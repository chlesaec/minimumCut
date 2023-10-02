ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

lazy val root = (project in file("."))
  .settings(
    name := "fibonacciHeap"
  )

lazy val junit = "org.junit.jupiter" % "junit-jupiter-api" % "5.9.3"
libraryDependencies += junit % Test
