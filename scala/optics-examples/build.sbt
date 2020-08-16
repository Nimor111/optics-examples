import Dependencies._

ThisBuild / scalaVersion     := "2.13.2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val monocleVersion = "2.0.0" // depends on cats 2.x

lazy val root = (project in file("."))
  .settings(
    name := "optics-examples",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-law" % monocleVersion % "test"
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
