val scala3Version = "3.2.0"

inThisBuild(
  List(
    version := "1.0.0",
    scalaVersion := scala3Version,
    scalafmtOnCompile := true
  )
)

lazy val root = project
  .in(file("."))
  .aggregate(shared, d1, d2)
  .settings(
    name := "Advent of Code 2021"
  )

lazy val shared = project
  .in(file("shared"))
  .settings(
    name := "Shared"
  )

lazy val d1 = project
  .in(file("d1"))
  .dependsOn(shared)
  .settings(
    name := "Day 1"
  )

lazy val d2 = project
  .in(file("d2"))
  .dependsOn(shared)
  .settings(
    name := "Day 2"
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")
