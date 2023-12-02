val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "2-12",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
