val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "9-12",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature"),

    libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
        "ch.qos.logback" % "logback-classic" % "1.4.11",
        "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
