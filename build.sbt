import Dependencies._

ThisBuild / scalaVersion := "0.26.0-RC1"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file("."))
  .settings(
    name := "dotty-musings",
    // libraryDependencies ++= Seq(scalaTest).map(_ % Test),
    scalacOptions += "-language:implicitConversions"
  )
