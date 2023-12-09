lazy val root = project.in(file("."))
    .settings(
        name := "AdventOfCode2023",
        version := "0.1",
        scalaVersion := "3.3.1",
        libraryDependencies += "org.scala-lang" %% "scala3-staging" % scalaVersion.value,
        scalacOptions += "-Xcheck-macros"
    )

