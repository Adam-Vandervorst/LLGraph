ThisBuild / version := "0.3.3"
ThisBuild / organization := "be.adamv"
ThisBuild / scalaVersion := "3.2.2"
ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
root / Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.ScalaLibrary
parallelExecution := false
ThisBuild / scalacOptions += "-explain"
ThisBuild / scalacOptions += "-source future"

lazy val root = (project in file("."))
  .settings(
    name := "LLGraph"
  )

publishTo := Some(Resolver.file("local-ivy", file("~")))
