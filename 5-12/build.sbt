val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "5-12",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test,
                                "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
    )
  )
