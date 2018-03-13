name := "ScalaWithCats"

version := "0.1"

scalaVersion := "2.12.4"

libraryDependencies ++=
  Seq(
    "org.typelevel" %% "cats-core" % "1.0.1",
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test"  )
