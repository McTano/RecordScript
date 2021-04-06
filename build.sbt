lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.recordscript",
      scalaVersion := "2.13.3"
    )
  ),
  name := "recordscript"
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % Test
libraryDependencies += "com.lihaoyi" % "ammonite" % "2.3.8" % "test" cross CrossVersion.full
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
