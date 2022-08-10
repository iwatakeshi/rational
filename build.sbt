ThisBuild / organization := "co.prismify"
ThisBuild / scalaVersion := "3.1.2"
ThisBuild / version      := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "Rational"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test"