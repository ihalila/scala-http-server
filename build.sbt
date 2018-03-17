lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "la.hali",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "scala-http-server",

    wartremoverErrors ++= Warts.unsafe,

    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0",
    libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3",
    libraryDependencies += "co.fs2" %% "fs2-core" % "0.10.1",
    libraryDependencies += "co.fs2" %% "fs2-io" % "0.10.1",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % Test
  )
