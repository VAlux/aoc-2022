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
  .aggregate(shared, d1, d2, d3, d4, d5, d6, d7)
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

lazy val d3 = project
  .in(file("d3"))
  .dependsOn(shared)
  .settings(
    name := "Day 3"
  )

lazy val d4 = project
  .in(file("d4"))
  .dependsOn(shared)
  .settings(
    name := "Day 4"
  )

lazy val d5 = project
  .in(file("d5"))
  .dependsOn(shared)
  .settings(
    name := "Day 5"
  )

lazy val d6 = project
  .in(file("d6"))
  .dependsOn(shared)
  .settings(
    name := "Day 6"
  )

lazy val d7 = project
  .in(file("d7"))
  .dependsOn(shared)
  .settings(
    name := "Day 7"
  )

addCommandAlias("cd", "project")
addCommandAlias("ls", "projects")
addCommandAlias("c", "compile")
addCommandAlias("rel", "reload")
addCommandAlias("r", "run")
