import sbt._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "turtle"
ThisBuild / organizationName := "turtle"

lazy val root = (project in file("."))
  .settings(
    name := "turtle",
  )

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.24"